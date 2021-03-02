FLAGS<-flags(flag_integer('dense_units1',80),
             flag_integer('dense_units2',50),
             flag_integer('dense_units3',30),
             flag_numeric('dropout1',0.4),
             flag_numeric('dropout2',0.3),
             flag_numeric('dropout3',0.2))

model <- keras_model_sequential() %>%
  layer_dense(unit=FLAGS$dense_units1,activation='relu',input_shape = c(40)) %>%
  layer_dropout(rate=FLAGS$dropout1)%>%
  layer_dense(unit=FLAGS$dense_units2,activation='relu') %>%
  layer_dropout(rate=FLAGS$dropout2)%>%
  layer_dense(unit=FLAGS$dense_units3,activation='relu') %>%
  layer_dropout(rate=FLAGS$dropout3)%>%
  layer_dense(unit=2,activation='sigmoid')
summary(model)

model %>%
  compile(loss='binary_crossentropy',
          optimizer='adam',
          metrics='accuracy')

set.seed(42)
history<-model %>%
  fit(train,
      trainlable,
      epoch=30,
      batch_size=32,
      validation_split=0.2)














