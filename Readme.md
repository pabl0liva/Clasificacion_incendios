# Clasificación de tipos de incendios en función de la superficie afectada
## _Una aproximación mediante técnicas de Machine Learning_

### Descripción
Se han utilizado técnicas de aprendizaje automático para, asumido que en determinado lugar se va a declarar un incendio, encontrar un modelo de clasificación multiclase que sea capaz de predecir el tipo de incendio (número de hectáreas quemadas) que ocurriría en base a una serie de características dadas.

Además, se ha creado una aplicación en la que el usuario puede solicitar la predicción de superficie quemada en una provincia o región para los próximos 7 días y visualiza el resultado en un mapa interactivo como el que se muestra en la imagen.

![dashboard.png](https://www.dropbox.com/s/n6rv2t1z9dt3u2s/dashboard.png?dl=0&raw=1)

##### Evaluación
Este proyecto se ha presentado como Trabajo de Fin de Máster del máster en Big Data & Data Science impartido en la Universidad Complutense de Madrid en el curso 20/21, siendo evaluado con un **9**.

### Descripción del trabajo y origen de los datos
La mayor dificultad que nos hemos encontrado durante la ejecución del proyecto ha sido conseguir los datos con los que empezar a modelar, ya que hemos tenido que recurrir a diversas fuentes para lograr un conjunto de datos consistente y con variables diversas.

Nuestro punto de partida era el histórico de incendios ocurridos en España desde 2001 a 2015. Necesitábamos agregarle las mediciones meteorológicas reales que hubo en el momento del incendio. Para ello, primero hicimos una llamada a la API de AEMET y obtuvimos las medidas que tomaron durante esos 15 años, más de 1.2M de registros. Por otra parte, hicimos otra llamada a la API para obtener la recopilación de estaciones meteorológicas existentes.

Unir las estaciones con sus medidas fue sencillo, pero no había ningún tipo de identificador común con la base de datos de incendios. Para resolver esto creamos una función que calculaba las distancias entre cada incendio y todas las estaciones meteorológicas y le asignaba la más cercana. Así pudimos unir a cada incendio las variables medidas por su estación más cercana.

El inconveniente es que, de 82.600 incendios, 18.000 tenían datos nulos ya que su estación más cercana o no existía en el momento del incendio, o ese día no había tomado datos. Para resolverlo creamos un conjunto de datos propio con todas las estaciones y las estaciones más próximas o similares a ellas. De este modo, si un incendio tenía datos nulos por su estación más próxima le asignábamos los de la siguiente. 

![fuentes.png](https://www.dropbox.com/s/wbmb22uxocszefx/fuentes.png?dl=0&raw=1)

Aprovechamos la nueva base de datos creada para añadir al dataset las variables de zonas meteorológicas y el código de municipio que el INE asigna a cada municipio. Esta variable nos ha permitido posteriormente unir y combinar todos los datos. Del Instituto Geográfico Nacional (IGN) hemos conseguido la información asociada a cada municipio, así como su representación geospacial, y los hemos unido también al dataset original. El objetivo era tener el mayor número de variables útiles.

Una vez que tuvimos un conjunto de datos que nos convencía, se llevó a cabo un análisis profundo de los datos y su posterior limpieza, así como la creación de nuevas variables que pudieran ser útiles en la creación del modelo. Tras obtener el conjunto de datos final, realizamos una selección de variables basándonos en la correlación con la variable objetivo y la colinealidad existente entre algunas de las variables predictoras y así continuar con la elaboración de un modelo de clasificación multiclase por el cuál podremos saber la superficie quemada dadas unas condiciones.

Tras realizar el _feature engineering_ y seleccionadas las variables, se ha lanzado una selección de modelos de clasificación, incluyendo _KNeighborsClassifier_, _RandomForestClassifier_, _GradientBoostingClassifier_ y _XGBClassifier_, entre otros, utilizando la técnica de _cross-validation_. Hemos concluido que el modelo que mejor puede predecir nuestra variable objetivo es un _XGBoost_, este mejora el rendimiento en comparación con otros algoritmos y minimiza la velocidad de ejecución, importante cuando se trabaja con tantos datos. Además de incluir los hiperparámetros clásicos de boosting y del aprendizaje basado en árboles, incluye propios que ayudan a reducir el sobreajuste y aumentar la precisión.

Una vez elegido el modelo, se ha procedido, de nuevo, a la técnica de cross-validation para comprobar si se diera el caso de _over-fitting_. Con una profundidad de 10, podemos afirmar que es un resultado sin _over-fitting_, dado que los valores son muy parecidos, contando con una media de 0.7078 y una desviación estándar de 0.0591.

Por último, hemos automatizado el proceso para simular una aplicación real, mediante la cual hacemos una nueva llamada a la API de AEMET para obtener la predicción meteorológica de los próximos 7 días de los municipios deseados con el fin de obtener la superficie quemada para cada uno de ellos en cada día y poder visualizarlo e interactuar con los datos en Tableau.

### Tecnologías utilizadas
Para llevar a cabo el proyecto hemos utilizado, entre otras, las siguientes herramientas:
- Python en Jupyter Notebook
- R en RStudio
- API rest de AEMET
- Librerías: Pandas, Numpy, request, matplotlib, sklearn, xgboost, SMOTE, pickle, psych, dplyr, ranger, tidyr, caret, ranger, corrplot, ggplot2...
- Tableau

### Autores
**Pablo Oliva Gómez**
- [Perfil](https://github.com/pabl0liva "Pablo Oliva")
- [Email](mailto:pabloliva@gmail.com "¡Hola!")
- [LinkedIn](https://www.linkedin.com/in/pabloliva/ "Bienvenidos")

**Ricardo Afonso Spinola**
- [Perfil](https://github.com/ricardoafsp "Ricardo Afonso")
- [Email](mailto:rafonsospinola@gmail.com "¡Hola!")
- [LinkedIn](https://www.linkedin.com/in/ricardoafonsospinola/ "Bienvenidos")

**Nerea Gómez Miguel**
- [Email](mailto:nereagomezmiguel@outlook.es "¡Hola!")
- [LinkedIn](https://www.linkedin.com/in/nerea-g%C3%B3mez-miguel-a0512119a/ "Bienvenidos")

**Malena Gómez Carrancio**
- [Perfil](https://github.com/malenarodriguezcarrancio "Malena Gómez")
- [Email](mailto:carranciomalena@outlook.com "¡Hola!")
- [LinkedIn](https://www.linkedin.com/in/malenarodriguezcarrancio/ "Bienvenidos")
 
**Celia Regueiro Emperador**
- [Email](mailto:celiaregueiro@gmail.com "¡Hola!")
- [LinkedIn](https://www.linkedin.com/in/celia-regueiro-emperador/ "Bienvenidos")

**Samanta Palango**
- [Email](mailto:samanthacaiza93@gmail.com "¡Hola!")

### Notas
Los datos obtenidos son públicos y pertenecen a AEMET, al Ministerio de Agricultura, al Instituto Nacional de Estadística y al Instituto Geográfico Nacional.

Para la limpieza de datos hemos utilizado algunas funciones en R que nos facilitó el profesor Guillermo Villarino y que están recogidas en el fichero Funciones_R.R.

### Licencia
Este proyecto está bajo la Licencia MIT - mira el archivo [LICENSE.md](LICENSE.md) para detalles.
