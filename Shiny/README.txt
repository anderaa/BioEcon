
This is the directory of the Shiny project.

There is also a command line model in this directory.

rsconnect and www are directories used by the Shiny app.

To try shiny on local machine:
1. set working directory to where ui and server are saved
2. in console:
   > library shiny
   > runApp(getwd())

To deploy:
1. go to: https://www.shinyapps.io/admin/#/login
2. login
3. on left -> account -> tokens -> show secret, copy it
   rsconnect::setAccountInfo(name='',
			      token='',
			      secret='')
4. set wd to where ui and server are saved
5. in console:
   > library('rsconnect')
   > rsconnect::setAccountInfo(name='',
			        token='',
			        secret='')
   > rsconnect::deployApp(getwd())


