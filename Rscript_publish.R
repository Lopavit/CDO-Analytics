####################################### CDO Analytics Use Case #######################################


# Get the data
data = read.csv("D:/Users/lopavit_ma/OneDrive - Minor International PCL/Documents/Personal/Central/Case/sampled_cogs_category_sku_data.csv")
data$order_date = as.Date(data$order_date, "%d/%m/%Y")



#### Calculating discount elasticity

# Split into lists by SKU
data.splt = split(data, f = data$sku)


PE = data.frame(sapply(data.splt, function(x) {
  formula = lm(x$total_qty_ordered ~ x$total_discount_amount)
  pe = formula$coefficients[2] * mean(x$total_discount_amount) / mean(x$total_qty_ordered)
  return(as.numeric(pe))
}
)
)
names(PE) = 'Elasticity'
PE$sku = rownames(PE)

data = merge(data, PE, by = 'sku', all.x = T)

#write.csv(data, "D:/Users/lopavit_ma/OneDrive - Minor International PCL/Documents/Personal/Central/Case/sampled_cogs_category_sku_data_with_elasticity.csv")



### Calculating the point of diminishing return

# Selecting only elastic SKUs
sku_elastic = PE[PE$Elasticity > 1 & is.na(PE$Elasticity) == F, ]
sku_inelastic = PE[PE$Elasticity < 1 & is.na(PE$Elasticity) == F, ]
sku_inconclusive = PE[is.na(PE$Elasticity) == T, ]
data2 = data[data$sku %in% sku_elastic$sku, ]
data2.splt = split(data2, f = data2$sku)

diminishing_points = sapply(data2.splt, function(x) {
  tryCatch({
    # Fitting cubic function
    formula3 = lm(x$total_qty_ordered ~ poly(x$total_discount_amount, 3, raw = F))
    # use model to get predicted values
    pred <- predict(formula3)
    ix <- sort(x$total_discount_amount, index.return=T)$ix
    
    diminishing_point = (-1*formula3$coefficients[3])/(3*formula3$coefficients[4])
    return(diminishing_point)
  }, error = function(e){cat("ERROR:", conditionMessage(e), ".\n")}
  )
})

diminishing_points = data.frame(t(data.frame(diminishing_points[lapply(diminishing_points, length) > 0])))
names(diminishing_points) = 'optimal_discount'
diminishing_points$sku = substr(rownames(diminishing_points), 2, nchar(colnames(diminishing_points)))




# Plotting
# plot(test$total_discount_amount, test$total_qty_ordered, col='deepskyblue4',xlab='discount',main='q')
# lines(test$total_discount_amount[ix], pred[ix], col='red', lwd=2)