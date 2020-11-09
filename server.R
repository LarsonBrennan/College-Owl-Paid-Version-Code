library(firebase)
library(shiny)
library(devtools)
library(data.table)
library(dplyr)
library(rdrop2)
library(readr)
library(tidytext)
library(tidyr)
library(textdata)
library(ggplot2)
library(tidyverse)
library(quanteda)
library(SentimentAnalysis)
library(igraph)
library(ggraph)
library(plotly)
library(httr)
library(formattable)
library(stringi)
library(memoise)
library(widyr)
library(topicmodels)
library(syuzhet)
library(LDAvis)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(stringr)
library(topicmodels)
library(tokenizers)
library(readtext)
library(languageR)
library(zipfR)
library(shinyWidgets)
library(twitteR)
library(grid)
library(gridExtra)
library(shinyjs)
library(pdftools)
library(textreadr)
library(RPostgres)
library(RPostgreSQL)
options(scipen = 999)

PADDLE_PRODUCT_ID <- 620148

function(input, output, session) {

  addClass(selector = "body", class = "sidebar-collapse")

  f <- FirebaseUI$new()$set_providers(
    email = TRUE,
    google = TRUE
  )$launch()

  firebase_user <- reactive({
    f$req_sign_in()
    f$get_signed_in()$response
  })

  observeEvent(input$signout, {
    f$sign_out()
  })

  output$user_out <- renderUI({
    req(firebase_user())

    user <- firebase_user()
    tagList(
      div(class="card",
          img(src = user$photoURL, style="width:100%"),
          h2(paste("Welcome", user$displayName)),

          p(class="email", user$email),
          p(paste("Last login:",
                       as.POSIXct(as.numeric(user$lastLoginAt)/1000,
                                  origin = "1970-01-01"))),
          p(paste("Created: ",
                       as.POSIXct(as.numeric(user$createdAt)/1000,
                                  origin = "1970-01-01"))),
          h4(actionButton("signout", "Sign out", class = "btn-danger")),
          p()
      ))

  })

  subscriber <- reactive({
    req(firebase_user())
    o <- fb_document_get(paste0("subscriptions/", firebase_user()$uid))
    if(!is.null(o) && o$fields$status$stringValue == "deleted"){
      return(NULL)
    }
    o

  })

  subscriber_details <- reactive({
    req(subscriber())
    ss <- subscriber()

    tagList(
      div(class="card",
        h3("Subscription details"),
        p("Status:", ss$fields$status$stringValue),
        p(class="email", ss$fields$email$stringValue),
        p("Last update: ", ss$fields$event_time$stringValue),
        p("Next bill date:", ss$fields$next_bill_date$stringValue),
        p(a(href=ss$fields$update_url$stringValue,
          "Update your subscription",
                             icon = icon("credit-card"),
                             class = "btn-info")),
        " ",
        p(a(href=ss$fields$cancel_url$stringValue,
          "Cancel your subscription",
                      icon = icon("window-close"),
                      class = "btn-danger"))
        )
      )

  })

  output$subscriber <- renderUI({
    req(firebase_user())

    subscriber <- subscriber()
    if(is.null(subscriber)){
      return(tagList(
        div(class="card",
        h4("To see the content on this page please subscribe:",
          pdle_subscribe(PADDLE_PRODUCT_ID,
                         user_id = firebase_user()$uid,
                         email = firebase_user()$email,
                         success = "https://www.collegeowl.co/your-subscription-is-active/"),
          helpText("If you have subscribed already, make sure you have logged in with a method using the same email as your subscription"),
          helpText("Use coupon code 'writingSeason' to get a 25% discount off your first month of College Owl")

          )))
        )
    }
    subscriber_details()
  })

  output$paid_content <- renderPlot({
    req(subscriber())
    plot(iris)
  })


  countyData <- read.table(
    text = "School State University
  'Law Schools'    'Alabama' 'Faulkner University - Thomas Goode Jones School of Law'
  'Law Schools'    'Alabama' 'Samford University - Cumberland School of Law'
  'Law Schools'    'Alabama' 'University of Alabama School of Law'
  'Law Schools'    'Arizona' 'Arizona State University - Sandra Day OConnor College of Law'
  'Law Schools'    'Arizona' 'University of Arizona - James E. Rogers College of Law'
  'Law Schools'    'Arkansas' 'University of Arkansas at Little Rock - William H. Bowen School of Law'
  'Law Schools'        'Arkansas' 'University of Arkansas School of Law'
  'Law Schools'        'California' 'California Western School of Law'
  'Law Schools'        'California' 'Chapman University School of Law'
  'Law Schools'        'California' 'Golden Gate University School of Law'
  'Law Schools'        'California' 'Loyola Marymount University - Loyola Law School'
  'Law Schools'        'California' 'Pepperdine University School of Law'
  'Law Schools'        'California' 'Santa Clara University School of Law'
  'Law Schools'        'California' 'Southwestern University School of Law'
  'Law Schools'        'California' 'Stanford Law School'
  'Law Schools'        'California' 'University of California, Berkeley School of Law (Boalt Hall)'
  'Law Schools'        'California' 'University of California, Davis School of Law (King Hall)'
  'Law Schools'        'California' 'University of California, Hastings College of the Law'
  'Law Schools'        'California' 'University of California, Irvine School of Law'
  'Law Schools'        'California' 'University of California, Los Angeles School of Law'
  'Law Schools'        'California' 'University of San Diego School of Law'
  'Law Schools'        'California' 'University of San Francisco School of Law'
  'Law Schools'        'California' 'University of Southern California - Gould School of Law'
  'Law Schools'        'California' 'University of the Pacific - McGeorge School of Law'
  'Law Schools'        'California' 'Western State College of Law'
  'Law Schools'        'Colorado' 'University of Colorado School of Law'
  'Law Schools'        'Colorado' 'University of Denver - Sturm College of Law'
  'Law Schools'        'Connecticut' 'Quinnipiac University School of Law'
  'Law Schools'        'Connecticut' 'University of Connecticut School of Law'
  'Law Schools'        'Connecticut' 'Yale Law School'
   'Law Schools'       'Delaware' 'Delaware Law School'
   'Law Schools'       'District of Columbia' 'American University - Washington College of Law'
   'Law Schools'       'District of Columbia' 'Georgetown University Law Center'
   'Law Schools'       'District of Columbia' 'Howard University School of Law'
   'Law Schools'       'District of Columbia' 'The Catholic University of America - Columbus School of Law'
  'Law Schools'        'District of Columbia' 'The George Washington University Law School'
  'Law Schools'        'District of Columbia' 'University of the District of Columbia - David A. Clarke School of Law'
  'Law Schools'        'Florida' 'Ave Maria School of Law'
  'Law Schools'        'Florida' 'Barry University - Dwayne O. Andreas School of Law'
   'Law Schools'       'Florida' 'Florida A&M University College of Law'
   'Law Schools'       'Florida' 'Florida Coastal School of Law'
  'Law Schools'        'Florida' 'Florida International University College of Law'
  'Law Schools'        'Florida' 'Florida State University College of Law'
  'Law Schools'        'Florida' 'Nova Southeastern University - Shepard Broad Law Center'
  'Law Schools'        'Florida' 'St. Thomas University School of Law'
  'Law Schools'        'Florida' 'Stetson University College of Law'
   'Law Schools'       'Florida' 'University of Florida Levin College of Law'
   'Law Schools'       'Florida' 'University of Miami School of Law'
   'Law Schools'       'Georgia' 'Atlantas John Marshall Law School'
   'Law Schools'       'Georgia' 'Emory University School of Law'
   'Law Schools'       'Georgia' 'Georgia State University College of Law'
   'Law Schools'       'Georgia' 'Mercer University - Walter F. George School of Law'
   'Law Schools'       'Georgia' 'University of Georgia School of Law'
   'Law Schools'       'Hawaii' 'University of Hawaii - William S. Richardson School of Law'
   'Law Schools'       'Idaho' 'Concordia University School of Law'
   'Law Schools'       'Idaho' 'University of Idaho College of Law'
  'Law Schools'        'Illinois' 'DePaul University College of Law'
   'Law Schools'       'Illinois' 'Illinois Institute of Technology - Chicago-Kent College of Law'
   'Law Schools'       'Illinois' 'Loyola University Chicago School of Law'
   'Law Schools'       'Illinois' 'Northern Illinois University College of Law'
  'Law Schools'        'Illinois' 'Northwestern University School of Law'
  'Law Schools'        'Illinois' 'Southern Illinois University School of Law'
   'Law Schools'       'Illinois' 'UIC John Marshall Law School'
   'Law Schools'       'Illinois' 'University of Chicago Law School'
  'Law Schools'        'Illinois' 'University of Illinois College of Law'
  'Law Schools'        'Indiana' 'Indiana University - Robert H. McKinney School of Law'
  'Law Schools'        'Indiana' 'Indiana University Bloomington - Maurer School of Law'
  'Law Schools'        'Indiana' 'Notre Dame Law School'
  'Law Schools'        'Iowa' 'Drake University Law School'
  'Law Schools'        'Iowa' 'University of Iowa College of Law'
  'Law Schools'        'Kansas' 'University of Kansas School of Law'
  'Law Schools'        'Kansas' 'Washburn University School of Law'
  'Law Schools'        'Kentucky' 'Northern Kentucky University - Salmon P. Chase College of Law'
  'Law Schools'        'Kentucky' 'University of Kentucky College of Law'
  'Law Schools'        'Kentucky' 'University of Louisville - Louis D. Brandeis School of Law'
  'Law Schools'        'Louisiana' 'Louisiana State University - Paul M. Hebert Law Center'
  'Law Schools'        'Louisiana' 'Loyola University New Orleans College of Law'
  'Law Schools'        'Louisiana' 'Southern University Law Center'
  'Law Schools'        'Louisiana' 'Tulane University School of Law'
  'Law Schools'        'Maine' 'University of Maine School of Law'
   'Law Schools'       'Maryland' 'University of Baltimore School of Law'
   'Law Schools'       'Maryland' 'University of Maryland School of Law'
   'Law Schools'       'Massachusetts' 'Boston College Law School'
  'Law Schools'        'Massachusetts' 'Boston University School of Law'
  'Law Schools'        'Massachusetts' 'Harvard Law School'
  'Law Schools'        'Massachusetts' 'New England School of Law'
  'Law Schools'        'Massachusetts' 'Northeastern University School of Law'
  'Law Schools'        'Massachusetts' 'Suffolk University Law School'
  'Law Schools'        'Massachusetts' 'University of Massachusetts School of Law'
  'Law Schools'        'Massachusetts' 'Western New England University School of Law'
  'Law Schools'        'Michigan' 'Michigan State University College of Law'
  'Law Schools'        'Michigan' 'University of Detroit Mercy School of Law'
   'Law Schools'       'Michigan' 'University of Michigan Law School'
   'Law Schools'       'Michigan' 'Wayne State University Law School'
  'Law Schools'        'Michigan' 'Western Michigan University - Thomas M. Cooley Law School'
  'Law Schools'        'Minnesota' 'Mitchell Hamline School of Law'
  'Law Schools'        'Minnesota' 'University of Minnesota Law School'
  'Law Schools'        'Minnesota' 'University of St. Thomas School of Law'
  'Law Schools'        'Mississippi' 'Mississippi College School of Law'
  'Law Schools'        'Mississippi' 'University of Mississippi School of Law'
  'Law Schools'       'Missouri' 'Saint Louis University School of Law'
  'Law Schools'        'Missouri' 'University of Missouri - Kansas City School of Law'
  'Law Schools'        'Missouri' 'University of Missouri School of Law'
  'Law Schools'        'Missouri' 'Washington University School of Law'
  'Law Schools'        'Montana' 'University of Montana School of Law'
  'Law Schools'        'Nebraska' 'Creighton University School of Law'
  'Law Schools'        'Nebraska' 'University of Nebraska - Lincoln College of Law'
  'Law Schools'        'Nevada' 'University of Nevada, Las Vegas - William S. Boyd School of Law'
  'Law Schools'        'New Hampshire' 'University of New Hampshire School of Law'
  'Law Schools'        'New Jersey' 'Rutgers Law School'
  'Law Schools'        'New Jersey' 'Seton Hall University School of Law'
  'Law Schools'        'New Mexico' 'University of New Mexico School of Law'
  'Law Schools'        'New York' 'Brooklyn Law School'
  'Law Schools'        'New York' 'City University of New York School of Law'
  'Law Schools'        'New York' 'Columbia Law School'
  'Law Schools'        'New York' 'Cornell Law School'
  'Law Schools'        'New York' 'Fordham University School of Law'
  'Law Schools'        'New York' 'Hofstra University School of Law'
  'Law Schools'        'New York' 'New York Law School'
  'Law Schools'        'New York' 'New York University School of Law'
  'Law Schools'        'New York' 'Pace University School of Law'
  'Law Schools'        'New York' 'St. Johns University School of Law'
  'Law Schools'        'New York' 'Syracuse University College of Law'
  'Law Schools'        'New York' 'Touro College - Jacob D. Fuchsberg Law Center'
  'Law Schools'        'New York' 'Union University - Albany Law School'
  'Law Schools'        'New York' 'University at Buffalo Law School, SUNY'
  'Law Schools'        'New York' 'Yeshiva University - Benamin N. Cardozo School of Law'
  'Law Schools'        'North Carolina' 'Campbell University - Norman Adrian Wiggins School of Law'
  'Law Schools'        'North Carolina' 'Duke University School of Law'
  'Law Schools'        'North Carolina' 'Elon University School of Law'
  'Law Schools'        'North Carolina' 'North Carolina Central University School of Law'
  'Law Schools'        'North Carolina' 'University of North Carolina School of Law'
  'Law Schools'        'North Carolina' 'Wake Forest University School of Law'
  'Law Schools'        'North Dakota' 'University of North Dakota School of Law'
  'Law Schools'        'Ohio' 'Capital University Law School'
  'Law Schools'        'Ohio' 'Case Western Reserve University School of Law'
  'Law Schools'        'Ohio' 'Cleveland State University, Cleveland-Marshall College of Law'
  'Law Schools'        'Ohio' 'Ohio Northern University - Pettit College of Law'
  'Law Schools'        'Ohio' 'Ohio State University - Michael E. Moritz College of Law'
  'Law Schools'        'Ohio' 'University of Akron School of Law'
  'Law Schools'        'Ohio' 'University of Cincinnati College of Law'
  'Law Schools'        'Ohio' 'University of Dayton School of Law'
  'Law Schools'        'Ohio' 'University of Toledo College of Law'
  'Law Schools'        'Oklahoma' 'Oklahoma City University School of Law'
  'Law Schools'        'Oklahoma' 'University of Oklahoma College of Law'
  'Law Schools'        'Oklahoma' 'University of Tulsa College of Law'
  'Law Schools'        'Oregon' 'Lewis & Clark Law School'
  'Law Schools'        'Oregon' 'University of Oregon School of Law'
  'Law Schools'        'Oregon' 'Willamette University College of Law'
  'Law Schools'        'Pennsylvania' 'Drexel University School of Law'
  'Law Schools'        'Pennsylvania' 'Duquesne University School of Law'
  'Law Schools'        'Pennsylvania' 'Penn State University - Dickinson School of Law'
  'Law Schools'        'Pennsylvania' 'Penn State University - Penn State Law'
  'Law Schools'        'Pennsylvania' 'Temple University - Beasley School of Law'
  'Law Schools'        'Pennsylvania' 'University of Pennsylvania Law School'
  'Law Schools'        'Pennsylvania' 'University of Pittsburgh School of Law'
  'Law Schools'        'Pennsylvania' 'Villanova University School of Law'
  'Law Schools'        'Pennsylvania' 'Widener University Commonwealth Law School'
  'Law Schools'        'Rhode Island' 'Roger WIlliams University School of Law'
  'Law Schools'        'South Carolina' 'Charleston School of Law'
  'Law Schools'        'South Carolina' 'University of South Carolina School of Law'
  'Law Schools'        'South Dakota' 'University of South Dakota School of Law'
  'Law Schools'        'Tennessee' 'Belmont University College of Law'
  'Law Schools'        'Tennessee' 'Lincoln Memorial University - Duncan School of Law'
  'Law Schools'        'Tennessee' 'University of Memphis - Cecil C. Humphreys School of Law'
  'Law Schools'        'Tennessee' 'University of Tennessee College of Law'
  'Law Schools'        'Tennessee' 'Vanderbilt University Law School'
  'Law Schools'        'Texas' 'Baylor University - Baylor Law School'
  'Law Schools'        'Texas' 'South Texas College of Law'
  'Law Schools'        'Texas' 'Southern Methodist University - Dedman School of Law'
  'Law Schools'        'Texas' 'St. Marys University School of Law'
  'Law Schools'        'Texas' 'Texas A&M University School of Law'
  'Law Schools'        'Texas' 'Texas Southern University - Thurgood Marshall School of Law'
  'Law Schools'        'Texas' 'Texas Tech University School of Law'
  'Law Schools'        'Texas' 'University of Houston Law Center'
  'Law Schools'        'Texas' 'University of North Texas at Dallas College of Law'
  'Law Schools'        'Texas' 'University of Texas School of Law'
  'Law Schools'        'Utah' 'Brigham Young University - J. Reuben Clark Law School'
  'Law Schools'        'Utah' 'University of Utah - S.J. Quinney College of Law'
  'Law Schools'        'Vermont' 'Vermont Law School'
  'Law Schools'        'Virginia' 'Appalachian School of Law'
  'Law Schools'        'Virginia' 'George Mason University School of Law'
  'Law Schools'        'Virginia' 'Liberty University School of Law'
  'Law Schools'        'Virginia' 'Regent University School of Law'
  'Law Schools'        'Virginia' 'The College of William and Mary - Marshall-Wythe School of Law'
  'Law Schools'        'Virginia' 'University of Richmond School of Law'
  'Law Schools'        'Virginia' 'University of Virginia School of Law'
  'Law Schools'        'Virginia' 'Washington and Lee University School of Law'
  'Law Schools'        'Washington' 'Gonzaga University School of Law'
  'Law Schools'        'Washington' 'Seattle University School of Law'
  'Law Schools'        'Washington' 'University of Washington School of Law'
  'Law Schools'        'West Virginia' 'West Virginia University College of Law'
  'Law Schools'        'Wisconsin' 'Marquette University Law School'
  'Law Schools'        'Wisconsin' 'University of Wisconsin Law School'
  'Law Schools'        'Wyoming' 'University of Wyoming College of Law'
  'Medical Schools'     'Alabama' 'University of Alabama School of Medicine'
  'Medical Schools'        'Alabama' 'University of South Alabama College of Medicine'
  'Medical Schools'          'Arizona' 'Mayo Clinic Alix School of Medicine'
  'Medical Schools'          'Arizona' 'University of Arizona College of Medicine Phoenix'
  'Medical Schools'          'Arizona' 'University of Arizone College of Medicine - Tucson'
  'Medical Schools'          'Arkansas' 'UAMS College of Medicine'
  'Medical Schools'         'California' 'California Northstate University College of Medicine'
  'Medical Schools'         'California' 'California University of Science and Medicine'
  'Medical Schools'         'California' 'Charles R. Drew University of Medicine and Science'
  'Medical Schools'         'California' 'David Geffen School of Medicine at UCLA'
  'Medical Schools'         'California' 'Kaiser Permanente School of Medicine'
  'Medical Schools'         'California' 'Keck School of Medicine of University of Southern California'
  'Medical Schools'          'California' 'Loma Linda University School of Medicine'
  'Medical Schools'          'California' 'Stanford University School of Medicine'
  'Medical Schools'          'California' 'UCSF School of Medicine'
  'Medical Schools'          'California' 'University of California, Davis School of Medicine'
  'Medical Schools'          'California' 'University of California, Irvine School of Medicine'
  'Medical Schools'         'California' 'University of California, Riverside School of Medicine'
  'Medical Schools'         'California' 'University of California, San Diego School of Medicine'
  'Medical Schools'         'Colorado' 'University of Colorado School of Medicine'
  'Medical Schools'          'Connecticut' 'Frank H. Netter M.D. School of Medicine at Quinnipiac University'
  'Medical Schools'          'Connecticut' 'University of Connecticut School of Medicine'
  'Medical Schools'          'Connecticut' 'Yale School of Medicine'
  'Medical Schools'          'District of Columbia' 'George Washington University Medical School'
  'Medical Schools'          'District of Columbia' 'Georgetown University School of Medicine'
  'Medical Schools'          'District of Columbia' 'Howard University College of Medicine'
  'Medical Schools'          'Florida' 'Florida Atlantic University Charles E. Schmidt College of Medicine'
  'Medical Schools'          'Florida' 'Florida International University Herbert Wertheim College of Medicine'
  'Medical Schools'          'Florida' 'Florida State University College of Medicine'
  'Medical Schools'         'Florida' 'University of Central Florida College of Medicine'
  'Medical Schools'         'Florida' 'University of Florida College of Medicine'
  'Medical Schools'          'Florida' 'University of Miami Leonard M. Miller School of Medicine'
  'Medical Schools'          'Florida' 'University of South Florida College of Medicine'
  'Medical Schools'          'Georgia' 'Emory University School of Medicine'
  'Medical Schools'          'Georgia' 'Medical College of Georgia at Augusta University'
  'Medical Schools'          'Georgia' 'Mercer University School of Medicine'
  'Medical Schools'          'Georgia' 'Morehouse School of Medicine'
  'Medical Schools'          'Hawaii' 'University of Hawaii at Manoa John A. Burns School of Medicine'
  'Medical Schools'          'Illinois' 'Chicago Medical School of Rosalind Franklin University of Medicine and Science'
  'Medical Schools'          'Illinois' 'Loyola University Chicago Stritch School of Medicine'
  'Medical Schools'          'Illinois' 'Northwestern University Feinberg School of Medicine'
  'Medical Schools'          'Illinois' 'Rush Medical College'
  'Medical Schools'          'Illinois' 'Southern Illinois University School of Medicine'
  'Medical Schools'          'Illinois' 'University of Chicago Pritzker School of Medicine'
  'Medical Schools'          'Illinois' 'University of Illinois at Urbana-Champaign Carle Illinois College of Medicine'
  'Medical Schools'          'Illinois' 'University of Illinois College of Medicine'
  'Medical Schools'          'Indiana' 'Indiana University School of Medicine - Evansville'
  'Medical Schools'          'Indiana' 'Indiana University School of Medicine'
  'Medical Schools'          'Iowa' 'University of Iowa Roy J. and Lucille A. Carver College of Medicine'
  'Medical Schools'          'Kansas' 'University of Kansas School of Medicine'
  'Medical Schools'          'Kentucky' 'University of Kentucky College of Medicine'
  'Medical Schools'          'Kentucky' 'University of Louisville School of Medicine'
  'Medical Schools'          'Louisiana' 'Louisiana State University School of Medicine in New Orleans'
  'Medical Schools'          'Louisiana' 'Louisiana State University School of Medicine in Shreveport'
  'Medical Schools'          'Louisiana' 'Tulane University School of Medicine'
  'Medical Schools'          'Maryland' 'Johns Hopkins University School of Medicine'
  'Medical Schools'          'Maryland' 'Uniformed Services University of the Health Sciences F. Edward Hebert School of Medicine'
  'Medical Schools'          'Maryland' 'University of Maryland School of Medicine'
  'Medical Schools'          'Massachusetts' 'Boston University School of Medicine'
  'Medical Schools'          'Massachusetts' 'Harvard Medical School'
  'Medical Schools'          'Massachusetts' 'Tufts University School of Medicine'
  'Medical Schools'          'Massachusetts' 'University of Massachusetts Medical School'
  'Medical Schools'          'Michigan' 'Central Michigan University College of Medicine'
  'Medical Schools'          'Michigan' 'Michigan State University College of Human Medicine'
  'Medical Schools'          'Michigan' 'Oakland University William Beaumont School of Medicine'
  'Medical Schools'          'Michigan' 'University of Michigan Medical School'
  'Medical Schools'          'Michigan' 'Wayne State University School of Medicine'
  'Medical Schools'          'Michigan' 'Western Michigan University Homer Stryker M.D. School of Medicine'
  'Medical Schools'          'Minnesota' 'Mayo Clinic College of Medicine'
  'Medical Schools'          'Minnesota' 'University of Minnesota Medical School'
  'Medical Schools'          'Mississippi' 'University of Mississippi School of Medicine'
  'Medical Schools'          'Missouri' 'Saint Louis University School of Medicine'
  'Medical Schools'          'Missouri' 'University of Missouri-Columbia School of Medicine'
  'Medical Schools'          'Missouri' 'University of Missouri–Kansas City School of Medicine'
  'Medical Schools'          'Missouri' 'Washington University School of Medicine'
  'Medical Schools'          'Nebraska' 'Creighton University School of Medicine'
  'Medical Schools'          'Nebraska' 'University of Nebraska College of Medicine'
  'Medical Schools'          'Nevada' 'University of Nevada, Las Vegas School of Medicine'
  'Medical Schools'         'Nevada' 'University of Nevada, Reno School of Medicine'
  'Medical Schools'         'New Hampshire' 'Dartmouth College Geisel School of Medicine'
  'Medical Schools'          'New Jersey' 'Cooper Medical School of Rowan University'
  'Medical Schools'          'New Jersey' 'Hackensack Meridian School of Medicine at Seton Hall University'
  'Medical Schools'          'New Jersey' 'Rutgers New Jersey Medical School'
  'Medical Schools'          'New Jersey' 'Rutgers Robert Wood Johnson Medical School'
  'Medical Schools'          'New Mexico' 'University of New Mexico School of Medicine'
  'Medical Schools'          'New York' 'Albany Medical College'
  'Medical Schools'          'New York' 'Albert Einstein College of Medicine'
  'Medical Schools'          'New York' 'Columbia University Roy and Diana Vagelos College of Physicians and Surgeons'
  'Medical Schools'          'New York' 'CUNY School of Medicine'
  'Medical Schools'          'New York' 'Donald and Barbara School of Medicine at Hofstra/Northwell'
  'Medical Schools'          'New York' 'Icahn School of Medicine at Mount Sinai'
  'Medical Schools'          'New York' 'Jacobs School of Medicine and Biomedical Sciences, University at Buffalo'
  'Medical Schools'          'New York' 'New York Medical College'
  'Medical Schools'          'New York' 'New York University Long Island School of Medicine'
  'Medical Schools'          'New York' 'New York University School of Medicine'
  'Medical Schools'          'New York' 'State University of New York Downstate Medical Center College of Medicine'
  'Medical Schools'          'New York' 'State University of New York Upstate Medical University'
  'Medical Schools'          'New York' 'Stony Brook University School of Medicine'
  'Medical Schools'          'New York' 'University of Rochester School of Medicine and Dentistry'
  'Medical Schools'          'New York' 'Weill Cornell Medical College'
  'Medical Schools'          'North Carolina' 'Duke University School of Medicine'
  'Medical Schools'          'North Carolina' 'The Brody School of Medicine at East Carolina University'
  'Medical Schools'          'North Carolina' 'University of North Carolina School of Medicine'
  'Medical Schools'          'North Carolina' 'Wake Forest School of Medicine'
  'Medical Schools'          'North Dakota' 'University of North Dakota School of Medicine and Health Sciences'
  'Medical Schools'          'Ohio' 'Boonshoft School of Medicine at Wright State University'
  'Medical Schools'          'Ohio' 'Case Western Reserve University School of Medicine'
  'Medical Schools'          'Ohio' 'Cleveland Clinic Lerner College of Medicine'
  'Medical Schools'          'Ohio' 'Northeast Ohio Medical University College of Medicine'
  'Medical Schools'          'Ohio' 'The Ohio State University College of Medicine'
  'Medical Schools'          'Ohio' 'University of Cincinnati College of Medicine'
  'Medical Schools'          'Ohio' 'University of Toledo College of Medicine'
  'Medical Schools'          'Oklahoma' 'University of Oklahoma College of Medicine'
  'Medical Schools'          'Oklahoma' 'University of Oklahoma School of Community Medicine'
  'Medical Schools'          'Oregon' 'Oregon Health & Science University School of Medicine'
  'Medical Schools'          'Pennsylvania' 'Drexel University College of Medicine'
  'Medical Schools'          'Pennsylvania' 'Geisinger Commonwealth School of Medicine'
  'Medical Schools'         'Pennsylvania' 'Lewis Katz School of Medicine, Temple University'
  'Medical Schools'         'Pennsylvania' 'Pennsylvania State University College of Medicine'
  'Medical Schools'          'Pennsylvania' 'Perelman School of Medicine at the University of Pennsylvania'
  'Medical Schools'          'Pennsylvania' 'Sidney Kimmel Medical College at Thomas Jefferson University'
  'Medical Schools'          'Pennsylvania' 'University of Pittsburgh School of Medicine'
  'Medical Schools'          'Rhode Island' 'Alpert Medical School at Brown University'
  'Medical Schools'          'South Carolina' 'Medical University of South Carolina College of Medicine'
  'Medical Schools'          'South Carolina' 'University of South Carolina School of Medicine Greenville'
  'Medical Schools'          'South Carolina' 'University of South Carolina School of Medicine'
  'Medical Schools'          'South Dakota' 'Sanford School of Medicine of the University of South Dakota'
  'Medical Schools'          'Tennessee' 'East Tennessee State University James H. Quillen College of Medicine'
  'Medical Schools'          'Tennessee' 'Meharry Medical College School of Medicine'
  'Medical Schools'          'Tennessee' 'University of Tennessee College of Medicine'
  'Medical Schools'          'Tennessee' 'Vanderbilt University School of Medicine'
  'Medical Schools'         'Texas' 'Baylor College of Medicine'
  'Medical Schools'         'Texas' 'Dell Medical School at The University of Texas at Austin'
  'Medical Schools'          'Texas' 'TCU and UNTHSC School of Medicine'
  'Medical Schools'          'Texas' 'Texas A&M Health Science Center College of Medicine'
  'Medical Schools'          'Texas' 'Texas Tech University Health Sciences Center Paul L. Foster School of Medicine'
  'Medical Schools'          'Texas' 'Texas Tech University Health Sciences Center School of Medicine'
  'Medical Schools'          'Texas' 'University of Houston'
  'Medical Schools'          'Texas' 'University of Texas Medical Branch School of Medicine'
  'Medical Schools'          'Texas' 'University of Texas Rio Grande Valley School of Medicine'
  'Medical Schools'          'Texas' 'University of Texas Southwestern Medical School at Dallas'
  'Medical Schools'          'Texas' 'UT Health San Antonio Joe R. and Teresa Lozano Long School of Medicine'
  'Medical Schools'          'Texas' 'UTHealth John P. and Katherine G. McGovern Medical School'
  'Medical Schools'          'Utah' 'University of Utah School of Medicine'
  'Medical Schools'          'Vermont' 'University of Vermont College of Medicine'
  'Medical Schools'          'Virginia' 'Eastern Virginia Medical School'
  'Medical Schools'         'Virginia' 'University of Virginia School of Medicine'
  'Medical Schools'          'Virginia' 'VCU School of Medicine, Medical College of Virginia Health Sciences Division'
  'Medical Schools'          'Virginia' 'Virginia Tech Carilion School of Medicine and Research Institute'
  'Medical Schools'          'Washington' 'University of Washington School of Medicine'
  'Medical Schools'          'Washington' 'Washington State University Elson S. Floyd College of Medicine'
  'Medical Schools'          'West Virginia' 'Joan C. Edwards School of Medicine at Marshall University'
  'Medical Schools'          'West Virginia' 'West Virginia University School of Medicine'
  'Medical Schools'          'Wisconsin' 'Medical College of Wisconsin'
  'Medical Schools'          'Wisconsin' 'University of Wisconsin School of Medicine and Public Health'
'Business Schools'  'Alabama' 'Auburn University - Raymond J. Harbert College of Business'
'Business Schools'    'Alabama' 'University of Alabama - Manderson Graduate School of Business'
'Business Schools'      'Arizona' 'Arizona State University - W.P. Carey School of Medicine'
'Business Schools'      'Arizona' 'Northern Arizona University - W.A. Franke College of Business'
'Business Schools'      'Arizona' 'University of Arizona - Eller College of Management'
'Business Schools'      'California' 'Chapman University - Argyros School of Business and Economics'
'Business Schools'      'California' 'Pepperdine University - Graziadio School of Business and Management'
'Business Schools'      'California' 'Stanford University - Stanford Graduate School of Business'
'Business Schools'      'California' 'University of California Riverside - A. Gary Anderson Graduate School of Management'
'Business Schools'      'California' 'University of California, Berkeley - Haas School of Business'
'Business Schools'      'California' 'University of California, Davis - UC Davis Graduate School of Management'
'Business Schools'      'California' 'University of California, Irvine - Merage School of Business'
'Business Schools'      'California' 'University of California, Los Angeles - UCLA Anderson School of Management'
'Business Schools'      'California' 'University of California, San Diego - Rady School of Management'
'Business Schools'      'California' 'University of San Diego - School of Business'
'Business Schools'      'California' 'University of San Francisco - School of Management'
'Business Schools'      'California' 'University of Southern California - Marshall School of Business'
'Business Schools'      'Colorado' 'University of Colorado Boulder - Leeds School of Business'
'Business Schools'      'Colorado' 'University of Colorado Denver - CU Denver Business School'
'Business Schools'      'Colorado' 'University of Denver - Daniels College of Business'
'Business Schools'      'Connecticut' 'Quinnipiac University - School of Business'
'Business Schools'      'Connecticut' 'University of Connecticut - University of Connecticut School of Business'
'Business Schools'      'Connecticut' 'Yale University - Yale School of Management'
'Business Schools'      'Delaware' 'University of Delaware - Alfred Lerner College of Business and Economics'
'Business Schools'      'District of Columbia' 'American University - Kogod School of Business'
'Business Schools'      'District of Columbia' 'George Washington University - The George Washington University School of Business'
'Business Schools'      'District of Columbia' 'Georgetown University - McDonough School of Business'
'Business Schools'      'District of Columbia' 'Howard University - Howard University School of Business'
'Business Schools'      'Florida' 'Florida State University - Florida State University College of Business'
'Business Schools'      'Florida' 'University of Florida - Warrington College of Business'
'Business Schools'      'Florida' 'University of Miami - University of Miami Business School'
'Business Schools'      'Florida' 'University of South Florida - Muma College of Business'
'Business Schools'      'Georgia' 'Emory University - Goizueta Business School'
'Business Schools'      'Georgia' 'Georgia Institute of Technology - Scheller College of Business'
'Business Schools'      'Georgia' 'University of Georgia - Terry College of Business'
'Business Schools'      'Idaho' 'Boise State University - College of Business and Economics'
'Business Schools'      'Illinois' 'Northwestern University - Kellogg School of Management'
'Business Schools'      'Illinois' 'Southern Illinois University Carbondale - College of Business'
'Business Schools'      'Illinois' 'University of Chicago - Booth School of Business'
'Business Schools'      'Indiana' 'Indiana University - Kelley School of Business'
'Business Schools'      'Indiana' 'Purdue University - Krannert School of Management'
'Business Schools'      'Indiana' 'University of Notre Dame - Mendoza College of Business'
'Business Schools'      'Iowa' 'Iowa State University - Iowa State University College of Business'
'Business Schools'      'Kansas' 'University of Kansas - School of Business'
'Business Schools'      'Kentucky' 'University of Kentucky - Gatton College of Business and Economics'
'Business Schools'      'Kentucky' 'University of Louisville'
'Business Schools'      'Louisiana' 'Louisiana State University - E.J. Ourso College of Business'
'Business Schools'      'Louisiana' 'Louisiana Tech University - College of Business'
'Business Schools'      'Louisiana' 'Tulane University - Freeman School of Business'
'Business Schools'      'Maryland' 'Morgan State University - Earl G. Graves School of Business & Management'
'Business Schools'      'Maryland' 'University of Maryland, College Park - Robert H. Smith School of Business'
'Business Schools'      'Massachusetts' 'Babson College - F.W. Olin Graduate School of Business'
'Business Schools'      'Massachusetts' 'Bentley University - McCallum Graduate School of Business'
'Business Schools'      'Massachusetts' 'Boston College - Carroll School of Management'
'Business Schools'      'Massachusetts' 'Boston University - Questrom School of Business'
'Business Schools'      'Massachusetts' 'Clark University - School of Management'
'Business Schools'      'Massachusetts' 'Harvard University - Harvard Business School'
'Business Schools'      'Massachusetts' 'Massachusetts Institute of Technology - MIT Sloan School of Management'
'Business Schools'      'Massachusetts' 'Northeastern University - DAmore McKim School of Business'
'Business Schools'      'Massachusetts' 'Suffolk University - Sawyer Business School'
'Business Schools'      'Massachusetts' 'University of Massachusetts Amherst - Isenberg School of Management'
'Business Schools'      'Michigan' 'Michigan State University - Eli Broad College of Business'
'Business Schools'      'Michigan' 'University of Michigan - Ross School of Business'
'Business Schools'      'Minnesota' 'University of Minnesota - Carlson School of Management'
'Business Schools'      'Mississippi' 'University of Mississippi - University of Mississippi School of Business Administration'
'Business Schools'      'Mississippi' 'University of Southern Mississippi - College of Business and Economic Development'
'Business Schools'      'Missouri' 'University of Missouri - Trulaske College of Business'
'Business Schools'      'Missouri' 'Washington University in St. Louis - Olin Business School'
'Business Schools'      'New Hampshire' 'Dartmouth College - Tuck School of Business'
'Business Schools'      'New Jersey' 'New Jersey Institute of Technology - Martin Tuchman School of Management'
'Business Schools'      'New Jersey' 'Rutgers University - Rutgers Business School'
'Business Schools'      'New Jersey' 'Stevens Institute of Technology - School of Business'
'Business Schools'      'New York' 'Binghamton University - Binghamton University School of Management'
'Business Schools'      'New York' 'Clarkson University - School of Business'
'Business Schools'      'New York' 'Columbia University - Columbia Business School'
 'Business Schools'     'New York' 'Cornell University - Samuel Curtis Johnson Graduate School of Management'
 'Business Schools'     'New York' 'CUNY Baruch College - Zicklin School of Business'
 'Business Schools'     'New York' 'Fordham University - Gabelli School of Business'
 'Business Schools'     'New York' 'Hofstra University - Zarb School of Business'
'Business Schools'      'New York' 'New York University - Stern School of Business'
'Business Schools'      'New York' 'Pace University - Lubin School of Business'
'Business Schools'      'New York' 'Rochester Institute of Technology - E. Philip Saunders College of Business'
'Business Schools'      'New York' 'St. John Fisher College - School of Business'
'Business Schools'      'New York' 'SUNY Albany - Massry Center for Business'
'Business Schools'      'New York' 'SUNY Buffalo - University at Buffalo School of Management'
'Business Schools'      'New York' 'Syracuse University - Martin J. Whitman School of Management'
'Business Schools'      'New York' 'University of Rochester - Simon Business School'
'Business Schools'      'North Carolina' 'Appalachian State University - Walker College of Business'
'Business Schools'      'North Carolina' 'Duke University - Fuqua School of Business'
'Business Schools'      'North Carolina' 'North Carolina A&T State University - Wille A. Deese College of Business and Economics'
'Business Schools'      'North Carolina' 'North Carolina State University - Poole College of Management'
'Business Schools'      'North Carolina' 'University of North Carolina, Chapel Hill - Kenan-Flagler Business School'
 'Business Schools'     'North Carolina' 'University of North Carolina, Greensboro - Bryan School of Business and Economics'
 'Business Schools'     'Ohio' 'Case Western Reserve University - Weatherhead School of Management'
 'Business Schools'     'Ohio' 'John Carroll University - Boler College of Business'
 'Business Schools'     'Ohio' 'Ohio State University - Fisher College of Business'
'Business Schools'      'Ohio' 'University of Cincinnati - Carl H. Lindner College of Business'
'Business Schools'      'Oklahoma' 'Oklahoma State University - Spears School of Business'
'Business Schools'      'Oklahoma' 'University of Oklahoma - Michael F. Price College of Business'
'Business Schools'      'Oregon' 'Oregon State University - College of Business'
'Business Schools'      'Oregon' 'University of Oregon - Lundquist College of Business'
 'Business Schools'     'Oregon' 'Willamette University - Atkinson Graduate School of Management'
 'Business Schools'     'Pennsylvania' 'Carnegie Mellon University - Tepper School of Business'
'Business Schools'      'Pennsylvania' 'La Salle University - School of Business'
'Business Schools'      'Pennsylvania' 'Penn State University - Smeal College of Business'
'Business Schools'      'Pennsylvania' 'Temple University - Fox School of Business'
'Business Schools'      'Pennsylvania' 'University of Pennsylvania - The Wharton School'
'Business Schools'      'Pennsylvania' 'University of Pittsburgh - Katz School of Business'
'Business Schools'      'South Carolina' 'Clemson University - College of Business'
'Business Schools'      'South Carolina' 'College of Charleston - School of Business'
'Business Schools'      'South Carolina' 'University of South Carolina - Moore School of Business'
 'Business Schools'     'South Dakota' 'University of South Dakota - Beacom School of Business'
 'Business Schools'     'Tennessee' 'University of Tennessee - Haslam College of Business'
 'Business Schools'     'Tennessee' 'Vanderbilt University - Owen Graduate School of Manangement'
 'Business Schools'     'Texas' 'Baylor University - Hankamer School of Business'
'Business Schools'      'Texas' 'Rice University - Jesse H. Jones Graduate School of Management'
'Business Schools'      'Texas' 'Southern Methodist University - Cox School of Business'
'Business Schools'      'Texas' 'Texas A&M University - Mays Business School'
'Business Schools'      'Texas' 'Texas Christian University - Neeley School of Business'
'Business Schools'     'Texas' 'Texas Tech University - Rawls College of Business'
'Business Schools'      'Texas' 'University of Houston - Bauer College of Business'
'Business Schools'      'Texas' 'University of Texas at Austin - McCombs School of Business'
'Business Schools'     'Texas' 'University of Texas at Dallas - Naveen Jindal School of Management'
'Business Schools'     'Texas' 'University of Texas at San Antonio - College of Business'
'Business Schools'      'Texas' 'West Texas A&M University - Paul and Virginia Engler Colllege of Business'
'Business Schools'      'Utah' 'Brigham Young University - Marriott School of Management'
'Business Schools'      'Utah' 'University of Utah - David Eccles School of Business'
'Business Schools'      'Virginia' 'College of William & Mary - Mason School of Business'
'Business Schools'      'Virginia' 'University of Virginia - Darden Graduate School of Business Administration'
'Business Schools'      'Washington' 'University of Washington - Foster School of Business'
'Business Schools'      'Wisconsin' 'University of Wisconsin at Madison - Wisconsin School of Business'
'Undergraduate Schools'   'Alabama' 'Auburn University'
'Undergraduate Schools'   'Alabama' 'Birmingham Southern College'
'Undergraduate Schools'   'Alabama' 'Judson College'
'Undergraduate Schools'   'Alabama' 'Samford University'
'Undergraduate Schools'   'Alabama' 'Spring Hill College'
'Undergraduate Schools'   'Alabama' 'Stillman College'
'Undergraduate Schools'   'Alabama' 'Talledega College'
'Undergraduate Schools'   'Alabama' 'University of Alabama-Birmingham'
'Undergraduate Schools'   'Alabama' 'University of Alabama-Huntsville'
'Undergraduate Schools'   'Alabama' 'University of Alabama-Tuscaloosa'
'Undergraduate Schools'   'Alabama' 'University of South Alabama'
'Undergraduate Schools'  'Alaska' 'University of Alaska - Fairbanks'
'Undergraduate Schools'  'Arizona' 'Arizona State University'
'Undergraduate Schools'  'Arizona' 'Grand Canyon University'
'Undergraduate Schools'  'Arizona' 'Northern Arizona University'
'Undergraduate Schools'  'Arizona' 'University of Arizona'
'Undergraduate Schools'  'Arkansas' 'Arkansas State University'
'Undergraduate Schools'  'Arkansas' 'Harding University'
'Undergraduate Schools'  'Arkansas' 'Hendrix College'
'Undergraduate Schools'  'Arkansas' 'Lyon College'
'Undergraduate Schools'  'Arkansas' 'University of Arkansas - Fayetteville'
'Undergraduate Schools'  'Arkansas' 'University of Arkansas - LittleRock'
'Undergraduate Schools'  'Arkansas' 'University of Central Arkansas'
'Undergraduate Schools'  'Arkansas' 'William Baptist University'
'Undergraduate Schools'  'California' 'Azusa Pacific University'
'Undergraduate Schools'  'California' 'Biola University'
'Undergraduate Schools'  'California' 'Califonia Institute of Technology'
'Undergraduate Schools'  'California' 'Chapman University'
'Undergraduate Schools'  'California' 'Claremont McKenna College'
'Undergraduate Schools'  'California' 'Fresno State University'
'Undergraduate Schools'  'California' 'Harvey Mudd College'
'Undergraduate Schools'  'California' 'Loyola Marymount University'
'Undergraduate Schools'  'California' 'Marymount California University'
'Undergraduate Schools'  'California' 'Occidental College'
'Undergraduate Schools'  'California' 'Pepperdine University'
'Undergraduate Schools'  'California' 'Pitzer College'
'Undergraduate Schools'  'California' 'Ponoma College'
'Undergraduate Schools'  'California' 'Providence Christain College'
'Undergraduate Schools'  'California' 'San Diego State University'
'Undergraduate Schools'  'California' 'Santa Clara University'
'Undergraduate Schools'  'California' 'Scripps College'
'Undergraduate Schools'  'California' 'Soka University'
'Undergraduate Schools'  'California' 'Stanford University'
'Undergraduate Schools'  'California' 'Thomas Aquinas College'
'Undergraduate Schools'  'California' 'University of California - Berkeley'
'Undergraduate Schools'  'California' 'University of California - Davis'
'Undergraduate Schools'  'California' 'University of California - Irvine'
'Undergraduate Schools'  'California' 'University of California - Los Angeles'
'Undergraduate Schools'  'California' 'University of California - Merced'
'Undergraduate Schools'  'California' 'University of California - Riverside'
'Undergraduate Schools'  'California' 'University of California - SanDiego'
'Undergraduate Schools'  'California' 'University of California - Santa Barbara'
'Undergraduate Schools'  'California' 'University of California - Santa Cruz'
'Undergraduate Schools'  'California' 'University of La Verne'
'Undergraduate Schools'  'California' 'University of San Diego'
'Undergraduate Schools'  'California' 'University of San Francisco'
'Undergraduate Schools'  'California' 'University of Southern California'
'Undergraduate Schools'  'California' 'Universityof the West'
'Undergraduate Schools'  'California' 'Westmont College'
'Undergraduate Schools'  'California' 'Whittier College'
'Undergraduate Schools'  'Colorado' 'Colorado College'
'Undergraduate Schools'  'Colorado' 'Colorado School of Mines'
'Undergraduate Schools'  'Colorado' 'Colorado State University'
'Undergraduate Schools'  'Colorado' 'Fort Lewis College'
'Undergraduate Schools'  'Colorado' 'Regis University'
'Undergraduate Schools'  'Colorado' 'United States Air Force Academy'
'Undergraduate Schools'  'Colorado' 'University of Colorado - Boulder'
'Undergraduate Schools'  'Colorado' 'University of Colorado - Denver'
'Undergraduate Schools'  'Colorado' 'University of Colorado - Colorado Springs'
'Undergraduate Schools'  'Colorado' 'University of Denver'
'Undergraduate Schools'  'Colorado' 'University of Northern Colorado'
'Undergraduate Schools'  'Connecticut' 'Connecticut College'
'Undergraduate Schools'  'Connecticut' 'Quinnipiac University'
'Undergraduate Schools'  'Connecticut' 'Sacred Heart University'
'Undergraduate Schools'  'Connecticut' 'Trinity College'
'Undergraduate Schools'  'Connecticut' 'University of Bridgeport'
'Undergraduate Schools'  'Connecticut' 'University of Connecticut'
'Undergraduate Schools'  'Connecticut' 'University of Hartford'
'Undergraduate Schools'  'Connecticut' 'University of Saint Joseph'
'Undergraduate Schools'  'Connecticut' 'Wesleyan University'
'Undergraduate Schools'  'Connecticut' 'Yale University'
'Undergraduate Schools'    'Delaware' 'Delaware State University'
'Undergraduate Schools'  'Delaware' 'University of Delaware'
'Undergraduate Schools'  'District of Columbia' 'American University'
'Undergraduate Schools'  'District of Columbia' 'Catholic University'
'Undergraduate Schools'  'District of Columbia' 'Gallaudet University'
'Undergraduate Schools'  'District of Columbia' 'George Washington University'
'Undergraduate Schools'  'District of Columbia' 'Georgetown University'
'Undergraduate Schools'  'District of Columbia' 'Howard University'
'Undergraduate Schools'  'Florida' 'Ave Maria University'
'Undergraduate Schools'  'Florida' 'Barry University'
'Undergraduate Schools'  'Florida' 'Bethune Cookman University'
'Undergraduate Schools'  'Florida' 'Eckerd College'
'Undergraduate Schools'  'Florida' 'Florida Agricultural and Mechanical University'
'Undergraduate Schools'  'Florida' 'Florida Atlantic University'
'Undergraduate Schools'  'Florida' 'Florida Institute of Technology'
'Undergraduate Schools'  'Florida' 'Florida International University'
'Undergraduate Schools'  'Florida' 'Florida State University'
'Undergraduate Schools'  'Florida' 'Keiser University'
'Undergraduate Schools'  'Florida' 'New College of Florida'
'Undergraduate Schools'  'Florida' 'Nova Southeastern University'
'Undergraduate Schools'  'Florida' 'Palm Beach Atlantic University'
'Undergraduate Schools'  'Florida' 'University of Central Florida'
'Undergraduate Schools'  'Florida' 'University of Florida'
'Undergraduate Schools'  'Florida' 'University of Miami'
'Undergraduate Schools'  'Florida' 'University of North Florida'
'Undergraduate Schools'  'Florida' 'University of South Florida'
'Undergraduate Schools'  'Georgia' 'Agnes Scott College'
'Undergraduate Schools'  'Georgia' 'Augusta University'
'Undergraduate Schools'  'Georgia' 'Brewton Parker College'
'Undergraduate Schools'  'Georgia' 'Clack Atlanta University'
'Undergraduate Schools'  'Georgia' 'Covenant College'
'Undergraduate Schools'  'Georgia' 'Emory University'
'Undergraduate Schools'  'Georgia' 'Georgia Institute of Technology'
'Undergraduate Schools'  'Georgia' 'Georgia Southern University'
'Undergraduate Schools'  'Georgia' 'Georgia State University'
'Undergraduate Schools'  'Georgia' 'Kennesaw State University'
'Undergraduate Schools'  'Georgia' 'Mercer University'
'Undergraduate Schools'  'Georgia' 'Morehouse College'
'Undergraduate Schools'  'Georgia' 'Oglethorpe University'
'Undergraduate Schools'  'Georgia' 'Spelman College'
'Undergraduate Schools'  'Georgia' 'University of Georgia'
'Undergraduate Schools'  'Georgia' 'University of West Georgia'
'Undergraduate Schools'  'Georgia' 'Valdosta State University'
'Undergraduate Schools'  'Georgia' 'Wesleyan College'
'Undergraduate Schools'  'Georgia' 'Young Harris College'
'Undergraduate Schools'  'Hawaii' 'University of Hawaii - Hilo'
'Undergraduate Schools'  'Hawaii' 'University of Hawaii - Manoa'
'Undergraduate Schools'  'Idaho' 'Boise State University'
'Undergraduate Schools'  'Idaho' 'College of Idaho'
'Undergraduate Schools'  'Idaho' 'University of Idaho'
'Undergraduate Schools'  'Illinois' 'Augustana College'
'Undergraduate Schools'  'Illinois' 'Aurora University'
'Undergraduate Schools'  'Illinois' 'Benedictine University'
'Undergraduate Schools'  'Illinois' 'Blackburn College'
'Undergraduate Schools'  'Illinois' 'DePaul University'
'Undergraduate Schools'  'Illinois' 'East-West University Chicago'
'Undergraduate Schools'  'Illinois' 'Illinois College'
'Undergraduate Schools'  'Illinois' 'Illinois Institute of Technology'
'Undergraduate Schools'  'Illinois' 'Illinois State University'
'Undergraduate Schools'  'Illinois' 'Illinois Wesleyan University'
'Undergraduate Schools'  'Illinois' 'Knox College'
'Undergraduate Schools'  'Illinois' 'Lake Forest College'
'Undergraduate Schools'  'Illinois' 'Loyola University Chicago'
'Undergraduate Schools'  'Illinois' 'Monmouth College'
'Undergraduate Schools'  'Illinois' 'National Louis University'
'Undergraduate Schools'  'Illinois' 'Northern Illinois University'
'Undergraduate Schools'  'Illinois' 'Northwestern University'
'Undergraduate Schools'  'Illinois' 'Principia College'
'Undergraduate Schools'  'Illinois' 'Roosevelt University'
'Undergraduate Schools'  'Illinois' 'Southern Illinois University - Carbondale'
'Undergraduate Schools'  'Illinois' 'Southern Illinois University Edwardsville'
'Undergraduate Schools'  'Illinois' 'Trinity International University'
'Undergraduate Schools'  'Illinois' 'University of Chicago'
'Undergraduate Schools'  'Illinois' 'University of Illinois - Chicago'
'Undergraduate Schools'  'Illinois' 'University of Illinois - Urbana Champaign'
'Undergraduate Schools'  'Illinois' 'University of St. Francis'
'Undergraduate Schools'  'Illinois' 'Wheaton College'
'Undergraduate Schools'  'Indiana' 'Ball State University'
'Undergraduate Schools'  'Indiana' 'DePauw University'
'Undergraduate Schools'  'Indiana' 'Earlham College'
'Undergraduate Schools'  'Indiana' 'Franklin College'
'Undergraduate Schools'  'Indiana' 'Hanover College'
'Undergraduate Schools'  'Indiana' 'Indiana State University'
'Undergraduate Schools'  'Indiana' 'Indiana University - Bloomington'
'Undergraduate Schools'  'Indiana' 'Indiana University - Purdue University Indianapolis'
'Undergraduate Schools'  'Indiana' 'University of Notre Dame'
'Undergraduate Schools'  'Indiana' 'Purdue University'
'Undergraduate Schools'  'Indiana' 'Saint Marys College'
'Undergraduate Schools'  'Indiana' 'University of Indianapolis'
'Undergraduate Schools'  'Indiana' 'Valpo University'
'Undergraduate Schools'  'Indiana' 'Wabash College'
'Undergraduate Schools'  'Iowa' 'Central College'
'Undergraduate Schools'  'Iowa' 'Clarke University'
'Undergraduate Schools'  'Iowa' 'Coe College'
'Undergraduate Schools'  'Iowa' 'Cornell College'
'Undergraduate Schools'  'Iowa' 'Drake University'
'Undergraduate Schools'  'Iowa' 'Grinnell College'
'Undergraduate Schools'  'Iowa' 'Iowa State University'
'Undergraduate Schools'  'Iowa' 'Luther College'
'Undergraduate Schools'  'Iowa' 'Simpson College'
'Undergraduate Schools'  'Iowa' 'University of Iowa'
'Undergraduate Schools'  'Iowa' 'Wartburg College'
'Undergraduate Schools'  'Kansas' 'Baker University'
'Undergraduate Schools'  'Kansas' 'Kansas State University'
'Undergraduate Schools'  'Kansas' 'University Of Kansas'
'Undergraduate Schools'  'Kansas' 'Washburn University'
'Undergraduate Schools'  'Kansas' 'Wichita State University'
'Undergraduate Schools'  'Kentucky' 'Bellarmine University'
'Undergraduate Schools'  'Kentucky' 'Berea College'
'Undergraduate Schools'  'Kentucky' 'Centre College'
'Undergraduate Schools'  'Kentucky' 'Georgetown College'
'Undergraduate Schools'  'Kentucky' 'Northern Kentucky University'
'Undergraduate Schools'  'Kentucky' 'Spalding University'
'Undergraduate Schools'  'Kentucky' 'Transylvania University'
'Undergraduate Schools'  'Kentucky' 'University of Kentucky'
'Undergraduate Schools'  'Kentucky' 'University of Louisville'
'Undergraduate Schools'  'Kentucky' 'University of Pikeville'
'Undergraduate Schools'  'Kentucky' 'University of the Cumberlands'
'Undergraduate Schools'  'Kentucky' 'Western Kentucky University'
'Undergraduate Schools'  'Louisiana' 'Centenary College of Louisiana'
'Undergraduate Schools'  'Louisiana' 'Dillard University'
'Undergraduate Schools'  'Louisiana' 'Louisiana State University - Alexandria'
'Undergraduate Schools'  'Louisiana' 'Louisiana State University - Baton Rouge'
'Undergraduate Schools'  'Louisiana' 'Louisiana Tech University'
'Undergraduate Schools'  'Louisiana' 'Loyola University New Orleans'
'Undergraduate Schools'  'Louisiana' 'Tulane University'
'Undergraduate Schools'  'Louisiana' 'University of Louisiana - Lafayette'
'Undergraduate Schools'  'Louisiana' 'University of Louisiana - Monroe'
'Undergraduate Schools'  'Louisiana' 'University of New Orleans'
'Undergraduate Schools'  'Maine' 'Bates College'
'Undergraduate Schools'  'Maine' 'Bowdoin College'
'Undergraduate Schools'  'Maine' 'Colby Collge'
'Undergraduate Schools'  'Maine' 'College of the Atlantic'
'Undergraduate Schools'  'Maine' 'Husson University'
'Undergraduate Schools'  'Maine' 'University of Maine'
'Undergraduate Schools'  'Maine' 'University of New England'
'Undergraduate Schools'  'Maryland' 'Goucher College'
'Undergraduate Schools'  'Maryland' 'Johns Hopkins University'
'Undergraduate Schools'  'Maryland' 'Morgan State University'
'Undergraduate Schools'  'Maryland' 'St. Johns College'
'Undergraduate Schools'  'Maryland' 'St. Marys College of Maryland'
'Undergraduate Schools'  'Maryland' 'Towson University'
'Undergraduate Schools'  'Maryland' 'United States Naval Academy'
'Undergraduate Schools'  'Maryland' 'University of Maryland - Baltimore County'
'Undergraduate Schools'  'Maryland' 'University of Maryland - College Park'
'Undergraduate Schools'  'Maryland' 'University of Maryland - Eastern Shores'
'Undergraduate Schools'  'Maryland' 'Washington College'
'Undergraduate Schools'  'Massachusetts' 'Amherst College'
'Undergraduate Schools'  'Massachusetts' 'Boston College'
'Undergraduate Schools'  'Massachusetts' 'Boston University'
'Undergraduate Schools'  'Massachusetts' 'Brandeis University'
'Undergraduate Schools'  'Massachusetts' 'Clark University'
'Undergraduate Schools'  'Massachusetts' 'College of the Holy Cross'
'Undergraduate Schools'  'Massachusetts' 'Emmanuel College'
'Undergraduate Schools'  'Massachusetts' 'Gordon College'
'Undergraduate Schools'  'Massachusetts' 'Harvard University'
'Undergraduate Schools'  'Massachusetts' 'Lesley University'
'Undergraduate Schools'  'Massachusetts' 'Massachusetts College of Liberal Arts'
'Undergraduate Schools'  'Massachusetts' 'Massachusetts Institute of Technology'
'Undergraduate Schools'  'Massachusetts' 'Mount Holyoke College'
'Undergraduate Schools'  'Massachusetts' 'Northeastern University'
'Undergraduate Schools'  'Massachusetts' 'Pine Manor College'
'Undergraduate Schools'  'Massachusetts' 'Simmons University'
'Undergraduate Schools'  'Massachusetts' 'Smith College'
'Undergraduate Schools'  'Massachusetts' 'Stonehill College'
'Undergraduate Schools'  'Massachusetts' 'Tufts University'
'Undergraduate Schools'  'Massachusetts' 'University of Massachusetts - Amherst'
'Undergraduate Schools'  'Massachusetts' 'University of Massachusetts - Boston'
'Undergraduate Schools'  'Massachusetts' 'University of Massachusetts - Dartmouth'
'Undergraduate Schools'  'Massachusetts' 'University of Massachusetts - Lowell'
'Undergraduate Schools'  'Massachusetts' 'Wellesley College'
'Undergraduate Schools'  'Massachusetts' 'Western New England University'
'Undergraduate Schools'  'Massachusetts' 'Wheaton College'
'Undergraduate Schools'  'Massachusetts' 'Williams College'
'Undergraduate Schools'  'Massachusetts' 'Worcester Polytechnic Institute'
'Undergraduate Schools'  'Michigan' 'Albion College'
'Undergraduate Schools'  'Michigan' 'Andrews University'
'Undergraduate Schools'  'Michigan' 'Aquinas College'
'Undergraduate Schools'  'Michigan' 'Central Michigan University'
'Undergraduate Schools'  'Michigan' 'Eastern Michigan University'
'Undergraduate Schools'  'Michigan' 'Ferris State University'
'Undergraduate Schools'  'Michigan' 'Grand Valley State University'
'Undergraduate Schools'  'Michigan' 'Hillsdale College'
'Undergraduate Schools'  'Michigan' 'Hope College'
'Undergraduate Schools'  'Michigan' 'Kalamazoo College'
'Undergraduate Schools'  'Michigan' 'Michigan State University'
'Undergraduate Schools'  'Michigan' 'Michigan Technological University'
'Undergraduate Schools'  'Michigan' 'Oakland University'
'Undergraduate Schools'  'Michigan' 'University of Detroit Mercy'
'Undergraduate Schools'  'Michigan' 'University of Michigan - Ann Arbor'
'Undergraduate Schools'  'Michigan' 'University of Michigan - Flint'
'Undergraduate Schools'  'Michigan' 'Wayne State University'
'Undergraduate Schools'  'Michigan' 'Western Michigan University'
'Undergraduate Schools'  'Minnesota' 'Bethany Lutheran College'
'Undergraduate Schools'  'Minnesota' 'Bethel University'
'Undergraduate Schools'  'Minnesota' 'Carleton College'
'Undergraduate Schools'  'Minnesota' 'College of Saint Benedict & Saint Johns University'
'Undergraduate Schools'  'Minnesota' 'College of St. Scholastica'
'Undergraduate Schools'  'Minnesota' 'Concordia College'
'Undergraduate Schools'  'Minnesota' 'Gustavus Adolphus College'
'Undergraduate Schools'  'Minnesota' 'Macalester College'
'Undergraduate Schools'  'Minnesota' 'Metropolitan State University'
'Undergraduate Schools'  'Minnesota' 'St. Catherine University'
'Undergraduate Schools'  'Minnesota' 'St. Olaf College'
'Undergraduate Schools'  'Minnesota' 'University of Minnesota - Morris'
'Undergraduate Schools'  'Minnesota' 'University of Minnesota - Twin Cities'
'Undergraduate Schools'  'Minnesota' 'University of St. Thomas'
'Undergraduate Schools'  'Mississippi' 'Jackson State University'
'Undergraduate Schools'  'Mississippi' 'Millsaps College'
'Undergraduate Schools'  'Mississippi' 'Mississippi College'
'Undergraduate Schools'  'Mississippi' 'Mississippi State University'
'Undergraduate Schools'  'Mississippi' 'Tougaloo College'
'Undergraduate Schools'  'Mississippi' 'University of Mississippi'
'Undergraduate Schools'  'Mississippi' 'University of Southern Mississippi'
'Undergraduate Schools'  'Mississippi' 'William Carey University'
'Undergraduate Schools'  'Missouri' 'Lindenwood University'
'Undergraduate Schools'  'Missouri' 'Maryville University'
'Undergraduate Schools'  'Missouri' 'Missouri State University'
'Undergraduate Schools'  'Missouri' 'Missouri University of Science and Technology'
'Undergraduate Schools'  'Missouri' 'Saint Louis University'
'Undergraduate Schools'  'Missouri' 'University of Missouri - Columbia'
'Undergraduate Schools'  'Missouri' 'University of Missouri - Kansas City'
'Undergraduate Schools'  'Missouri' 'University of Missouri - St. Louis'
'Undergraduate Schools'  'Missouri' 'Washington University in St. Louis'
'Undergraduate Schools'  'Missouri' 'Westminster College'
'Undergraduate Schools'  'Missouri' 'William Woods University'
'Undergraduate Schools'  'Montana' 'Montana State University'
'Undergraduate Schools'  'Montana' 'University of Montana'
'Undergraduate Schools'  'Nebraska' 'Creighton University'
'Undergraduate Schools'  'Nebraska' 'University of Nebraska - Lincoln'
'Undergraduate Schools'  'Nebraska' 'University of Nebraska - Omaha'
'Undergraduate Schools'  'Nevada' 'University of Nevada - Las Vegas'
'Undergraduate Schools'  'Nevada' 'University of Nevada - Reno'
'Undergraduate Schools'  'New Hampshire' 'Dartmouth College'
'Undergraduate Schools'  'New Hampshire' 'Saint Anselm College'
'Undergraduate Schools'  'New Hampshire' 'University of New Hampshire'
'Undergraduate Schools'  'New Jersey' 'Bloomfield College'
'Undergraduate Schools'  'New Jersey' 'Drew University'
'Undergraduate Schools'  'New Jersey' 'Montclair State University'
'Undergraduate Schools'  'New Jersey' 'New Jersey Institute of Technology'
'Undergraduate Schools'  'New Jersey' 'Princeton University'
'Undergraduate Schools'  'New Jersey' 'Rowan University'
'Undergraduate Schools'  'New Jersey' 'Rutgers University - Camden'
'Undergraduate Schools'  'New Jersey' 'Rutgers University - New Brunswick'
'Undergraduate Schools'  'New Jersey' 'Rutgers University - Newark'
'Undergraduate Schools'  'New Jersey' 'Seton Hall University'
'Undergraduate Schools'  'New Jersey' 'Stevens Institute of Technology'
'Undergraduate Schools'  'New Mexico' 'New Mexico State University'
'Undergraduate Schools'  'New Mexico' 'St. Johns College'
'Undergraduate Schools'  'New Mexico' 'University of New Mexico'
'Undergraduate Schools'  'New York' 'Adelphi University'
'Undergraduate Schools'  'New York' 'Bard College'
'Undergraduate Schools'  'New York' 'Barnard College'
'Undergraduate Schools'  'New York' 'City College of New York'
'Undergraduate Schools'  'New York' 'Clarkson University'
'Undergraduate Schools'  'New York' 'Colgate University'
'Undergraduate Schools'  'New York' 'Columbia University'
'Undergraduate Schools'  'New York' 'Cornell University'
'Undergraduate Schools'  'New York' 'DYouville College'
'Undergraduate Schools'  'New York' 'Daemen College'
'Undergraduate Schools'  'New York' 'Fordham University'
'Undergraduate Schools'  'New York' 'Hamilton College'
'Undergraduate Schools'  'New York' 'Hartwick College'
'Undergraduate Schools'  'New York' 'Hobart and William Smith Colleges'
'Undergraduate Schools'  'New York' 'Hofstra University'
'Undergraduate Schools'  'New York' 'Houghton College'
'Undergraduate Schools'  'New York' 'Long Island University'
'Undergraduate Schools'  'New York' 'Marymount Manhattan College'
'Undergraduate Schools'  'New York' 'New York University'
'Undergraduate Schools'  'New York' 'Pace University'
'Undergraduate Schools'  'New York' 'Rensselaer Polytechnic Institute'
'Undergraduate Schools'  'New York' 'Rochester Institute of Technology'
'Undergraduate Schools'  'New York' 'Russell Sage College'
'Undergraduate Schools'  'New York' 'Sarah Lawrence College'
'Undergraduate Schools'  'New York' 'Skidmore College'
'Undergraduate Schools'  'New York' 'St. John Fisher College'
'Undergraduate Schools'  'New York' 'St. Johns University'
'Undergraduate Schools'  'New York' 'St. Lawrence University'
'Undergraduate Schools'  'New York' 'SUNY - Albany'
'Undergraduate Schools'  'New York' 'SUNY - Binghamton'
'Undergraduate Schools'  'New York' 'SUNY - Buffalo'
'Undergraduate Schools'  'New York' 'SUNY - College of Environmental Science and Forestry'
'Undergraduate Schools'  'New York' 'SUNY - Purchase'
'Undergraduate Schools'  'New York' 'SUNY - Stony Brook'
'Undergraduate Schools'  'New York' 'Syracuse University'
'Undergraduate Schools'  'New York' 'The New School'
'Undergraduate Schools'  'New York' 'Touro College'
'Undergraduate Schools'  'New York' 'Union College'
'Undergraduate Schools'  'New York' 'United States Military Academy West Point'
'Undergraduate Schools'  'New York' 'University of Rochester'
'Undergraduate Schools'  'New York' 'Vassar College'
'Undergraduate Schools'  'New York' 'Wells College'
'Undergraduate Schools'  'New York' 'Yeshiva University'
'Undergraduate Schools'  'North Carolina' 'Bennett College'
'Undergraduate Schools'  'North Carolina' 'Campbell University'
'Undergraduate Schools'  'North Carolina' 'Chowan University'
'Undergraduate Schools'  'North Carolina' 'Davidson College'
'Undergraduate Schools'  'North Carolina' 'Duke University'
'Undergraduate Schools'  'North Carolina' 'East Carolina University'
'Undergraduate Schools'  'North Carolina' 'Elon University'
'Undergraduate Schools'  'North Carolina' 'Gardner-Webb University'
'Undergraduate Schools'  'North Carolina' 'Guilford College'
'Undergraduate Schools'  'North Carolina' 'Johnson C. Smith University'
'Undergraduate Schools'  'North Carolina' 'Meredith College'
'Undergraduate Schools'  'North Carolina' 'North Carolina A&T State University'
'Undergraduate Schools'  'North Carolina' 'North Carolina State University'
'Undergraduate Schools'  'North Carolina' 'Salem College'
'Undergraduate Schools'  'North Carolina' 'University of North Carolina - Asheville'
'Undergraduate Schools'  'North Carolina' 'University of North Carolina - Chapel Hill'
'Undergraduate Schools'  'North Carolina' 'University of North Carolina - Charlotte'
'Undergraduate Schools'  'North Carolina' 'University of North Carolina - Greensboro'
'Undergraduate Schools'  'North Carolina' 'University of North Carolina - Wilmington'
'Undergraduate Schools'  'North Carolina' 'Wake Forest University'
'Undergraduate Schools'  'North Carolina' 'Warren Wilson College'
'Undergraduate Schools'  'North Carolina' 'Wingate University'
'Undergraduate Schools'  'North Dakota' 'North Dakota State University'
'Undergraduate Schools'  'North Dakota' 'University of Mary'
'Undergraduate Schools'  'North Dakota' 'University of North Dakota'
'Undergraduate Schools'  'Ohio' 'Bowling Green State University'
'Undergraduate Schools'  'Ohio' 'Case Western Reserve University'
'Undergraduate Schools'  'Ohio' 'Cleveland State University'
'Undergraduate Schools'  'Ohio' 'College of Wooster'
'Undergraduate Schools'  'Ohio' 'Denison University'
'Undergraduate Schools'  'Ohio' 'Kent State University'
'Undergraduate Schools'  'Ohio' 'Kenyon College'
'Undergraduate Schools'  'Ohio' 'Miami University'
'Undergraduate Schools'  'Ohio' 'Oberlin College'
'Undergraduate Schools'  'Ohio' 'Ohio State University'
'Undergraduate Schools'  'Ohio' 'Ohio University'
'Undergraduate Schools'  'Ohio' 'Ohio Wesleyan University'
'Undergraduate Schools'  'Ohio' 'Union Institute & University'
'Undergraduate Schools'  'Ohio' 'University of Akron'
'Undergraduate Schools'  'Ohio' 'University of Cincinnati'
'Undergraduate Schools'  'Ohio' 'University of Dayton'
'Undergraduate Schools'  'Ohio' 'University of Findlay'
'Undergraduate Schools'  'Ohio' 'University of Toledo'
'Undergraduate Schools'  'Ohio' 'Wittenberg University'
'Undergraduate Schools'  'Ohio' 'Wright State University'
'Undergraduate Schools'  'Oklahoma' 'Oklahoma City University'
'Undergraduate Schools'  'Oklahoma' 'Oklahoma State University'
'Undergraduate Schools'  'Oklahoma' 'University of Oklahoma'
'Undergraduate Schools'  'Oklahoma' 'University of Science and Arts of Oklahoma'
'Undergraduate Schools'  'Oklahoma' 'University of Tulsa'
'Undergraduate Schools'  'Oregon' 'George Fox University'
'Undergraduate Schools'  'Oregon' 'Lewis & Clark College'
'Undergraduate Schools'  'Oregon' 'Linfield College'
'Undergraduate Schools'  'Oregon' 'Oregon State University'
'Undergraduate Schools'  'Oregon' 'Pacific University'
'Undergraduate Schools'  'Oregon' 'Portland State University'
'Undergraduate Schools'  'Oregon' 'Reed College'
'Undergraduate Schools'  'Oregon' 'University of Oregon'
'Undergraduate Schools'  'Oregon' 'Willamette University'
'Undergraduate Schools'  'Pennsylvania' 'Albright College'
'Undergraduate Schools'  'Pennsylvania' 'Allegheny College'
'Undergraduate Schools'  'Pennsylvania' 'Bryn Athyn College'
'Undergraduate Schools'  'Pennsylvania' 'Bryn Mawr College'
'Undergraduate Schools'  'Pennsylvania' 'Bucknell University'
'Undergraduate Schools'  'Pennsylvania' 'Carnegie Mellon University'
'Undergraduate Schools'  'Pennsylvania' 'Chatham University'
'Undergraduate Schools'  'Pennsylvania' 'Cheyney University'
'Undergraduate Schools'  'Pennsylvania' 'Dickinson College'
'Undergraduate Schools'  'Pennsylvania' 'Drexel University'
'Undergraduate Schools'  'Pennsylvania' 'Duquesne University'
'Undergraduate Schools'  'Pennsylvania' 'Elizabethtown College'
'Undergraduate Schools'  'Pennsylvania' 'Franklin & Marshall College'
'Undergraduate Schools'  'Pennsylvania' 'Gannon University'
'Undergraduate Schools'  'Pennsylvania' 'Gettysburg College'
'Undergraduate Schools'  'Pennsylvania' 'Grove City College'
'Undergraduate Schools'  'Pennsylvania' 'Haverford College'
'Undergraduate Schools'  'Pennsylvania' 'Immaculata University'
'Undergraduate Schools'  'Pennsylvania' 'Indiana University of Pennsylvania'
'Undergraduate Schools'  'Pennsylvania' 'Jefferson University'
'Undergraduate Schools'  'Pennsylvania' 'Juniata College'
'Undergraduate Schools'  'Pennsylvania' 'Lafayette College'
'Undergraduate Schools'  'Pennsylvania' 'Lehigh University'
'Undergraduate Schools'  'Pennsylvania' 'Lycoming College'
'Undergraduate Schools'  'Pennsylvania' 'Mansfield University'
'Undergraduate Schools'  'Pennsylvania' 'Misericordia University'
'Undergraduate Schools'  'Pennsylvania' 'Moravian College'
'Undergraduate Schools'  'Pennsylvania' 'Muhlenberg College'
'Undergraduate Schools'  'Pennsylvania' 'Pennsylvania State University'
'Undergraduate Schools'  'Pennsylvania' 'Robert Morris University'
'Undergraduate Schools'  'Pennsylvania' 'Saint Vincent College'
'Undergraduate Schools'  'Pennsylvania' 'Susquehanna University'
'Undergraduate Schools'  'Pennsylvania' 'Swarthmore College'
'Undergraduate Schools'  'Pennsylvania' 'Temple University'
'Undergraduate Schools'  'Pennsylvania' 'University of Pennsylvania'
'Undergraduate Schools'  'Pennsylvania' 'University of Pittsburgh'
'Undergraduate Schools'  'Pennsylvania' 'Ursinus College'
'Undergraduate Schools'  'Pennsylvania' 'Villanova University'
'Undergraduate Schools'  'Pennsylvania' 'Washington & Jefferson College'
'Undergraduate Schools'  'Pennsylvania' 'Westminster College'
'Undergraduate Schools'  'Pennsylvania' 'Widener University'
'Undergraduate Schools'  'Pennsylvania' 'Wilkes University'
'Undergraduate Schools'  'Rhode Island' 'Brown University'
'Undergraduate Schools'  'Rhode Island' 'University of Rhode Island'
'Undergraduate Schools'  'South Carolina' 'Allen University'
'Undergraduate Schools'  'South Carolina' 'Clemson University'
'Undergraduate Schools'  'South Carolina' 'Furman University'
'Undergraduate Schools'  'South Carolina' 'Presby College'
'Undergraduate Schools'  'South Carolina' 'University of South Carolina - Beaufort'
'Undergraduate Schools'  'South Carolina' 'University of South Carolina - Columbia'
'Undergraduate Schools'  'South Carolina' 'Wofford College'
'Undergraduate Schools'  'South Dakota' 'South Dakota State University'
'Undergraduate Schools'  'South Dakota' 'University of South Dakota'
'Undergraduate Schools'  'Tennessee' 'Belmont University'
'Undergraduate Schools'  'Tennessee' 'Carson-Newman University'
'Undergraduate Schools'  'Tennessee' 'East Tennessee State University'
'Undergraduate Schools'  'Tennessee' 'Fisk University'
'Undergraduate Schools'  'Tennessee' 'Lane College'
'Undergraduate Schools'  'Tennessee' 'Lincoln Memorial University'
'Undergraduate Schools'  'Tennessee' 'Lipscomb University'
'Undergraduate Schools'  'Tennessee' 'Middle Tennessee State University'
'Undergraduate Schools'  'Tennessee' 'Rhodes College'
'Undergraduate Schools'  'Tennessee' 'Tennessee State University'
'Undergraduate Schools'  'Tennessee' 'Tennessee Tech University'
'Undergraduate Schools'  'Tennessee' 'Trevecca Nazarene University'
'Undergraduate Schools'  'Tennessee' 'Union University'
'Undergraduate Schools'  'Tennessee' 'University of Memphis'
'Undergraduate Schools'  'Tennessee' 'University of Tennessee - Chattanooga'
'Undergraduate Schools'  'Tennessee' 'University of Tennessee - Knoxville'
'Undergraduate Schools'  'Tennessee' 'University of the South'
'Undergraduate Schools'  'Tennessee' 'Vanderbilt University'
'Undergraduate Schools'  'Texas' 'Austin College'
'Undergraduate Schools'  'Texas' 'Baylor University'
'Undergraduate Schools'  'Texas' 'Dallas Baptist University'
'Undergraduate Schools'  'Texas' 'Lamar University'
'Undergraduate Schools'  'Texas' 'Our Lady of the Lake University'
'Undergraduate Schools'  'Texas' 'Rice University'
'Undergraduate Schools'  'Texas' 'Sam Houston State University'
'Undergraduate Schools'  'Texas' 'Southern Methodist University'
'Undergraduate Schools'  'Texas' 'Southwestern University'
'Undergraduate Schools'  'Texas' 'Stephen F. Austin State University'
'Undergraduate Schools'  'Texas' 'Texas A&M University - College Station'
'Undergraduate Schools'  'Texas' 'Texas A&M University - Commerce'
'Undergraduate Schools'  'Texas' 'Texas A&M University - Corpus Christi'
'Undergraduate Schools'  'Texas' 'Texas A&M University - Kingsville'
'Undergraduate Schools'  'Texas' 'Texas Christian University'
'Undergraduate Schools'  'Texas' 'Texas Southern University'
'Undergraduate Schools'  'Texas' 'Texas State University'
'Undergraduate Schools'  'Texas' 'Texas Tech University'
'Undergraduate Schools'  'Texas' 'Texas Wesleyan University'
'Undergraduate Schools'  'Texas' 'Texas Womans University'
'Undergraduate Schools'  'Texas' 'University of Houston'
'Undergraduate Schools'  'Texas' 'University of North Texas'
'Undergraduate Schools'  'Texas' 'University of Texas - Arlington'
'Undergraduate Schools'  'Texas' 'University of Texas - Austin'
'Undergraduate Schools'  'Texas' 'University of Texas - Dallas'
'Undergraduate Schools'  'Texas' 'University of Texas - El Paso'
'Undergraduate Schools'  'Texas' 'University of Texas - San Antonio'
'Undergraduate Schools'  'Texas' 'University of Texas - Tyler'
'Undergraduate Schools'  'Texas' 'University of Texas Rio Grande Valley'
'Undergraduate Schools'  'Texas' 'University of The Incarnate Word'
'Undergraduate Schools'  'Utah' 'Brigham Young University'
'Undergraduate Schools'  'Utah' 'University of Utah'
'Undergraduate Schools'  'Utah' 'Utah State University'
'Undergraduate Schools'  'Vermont' 'Bennington College'
'Undergraduate Schools'  'Vermont' 'Marlboro College'
'Undergraduate Schools'  'Vermont' 'Middlebury College'
'Undergraduate Schools'  'Vermont' 'Saint Michaels College'
'Undergraduate Schools'  'Vermont' 'University of Vermont'
'Undergraduate Schools'  'Virginia' 'Bridgewater College'
'Undergraduate Schools'  'Virginia' 'College of William and Mary'
'Undergraduate Schools'  'Virginia' 'Emory and Henry College'
'Undergraduate Schools'  'Virginia' 'George Mason University'
'Undergraduate Schools'  'Virginia' 'Hampden-Sydney College'
'Undergraduate Schools'  'Virginia' 'Hampton University'
'Undergraduate Schools'  'Virginia' 'Hollins University'
'Undergraduate Schools'  'Virginia' 'Liberty University'
'Undergraduate Schools'  'Virginia' 'Mary Baldwin University'
'Undergraduate Schools'  'Virginia' 'Old Dominion University'
'Undergraduate Schools'  'Virginia' 'Randolph College'
'Undergraduate Schools'  'Virginia' 'Randolph-Macon College'
'Undergraduate Schools'  'Virginia' 'Regent University'
'Undergraduate Schools'  'Virginia' 'Roanoke College'
'Undergraduate Schools'  'Virginia' 'Shenandoah University'
'Undergraduate Schools'  'Virginia' 'Southern Virginia University'
'Undergraduate Schools'  'Virginia' 'Sweet Briar College'
'Undergraduate Schools'  'Virginia' 'University of Richmond'
'Undergraduate Schools'  'Virginia' 'University of Virginia'
'Undergraduate Schools'  'Virginia' 'University of Virginias College at Wise'
'Undergraduate Schools'  'Virginia' 'Virginia Commonwealth University'
'Undergraduate Schools'  'Virginia' 'Virginia Military Institute'
'Undergraduate Schools'  'Virginia' 'Virginia Tech'
'Undergraduate Schools'  'Virginia' 'Virginia Union University'
'Undergraduate Schools'  'Virginia' 'Virginia Wesleyan University'
'Undergraduate Schools'  'Washington' 'Gonzaga University'
'Undergraduate Schools'  'Washington' 'Seattle Pacific University'
'Undergraduate Schools'  'Washington' 'Seattle University'
'Undergraduate Schools'  'Washington' 'University of Pugent Sound'
'Undergraduate Schools'  'Washington' 'University of Washington - Seattle'
'Undergraduate Schools'  'Washington' 'Washington State University - Pullman'
'Undergraduate Schools'  'Washington' 'Whitman College'
'Undergraduate Schools'  'West Virginia' 'Bethany College'
'Undergraduate Schools'  'West Virginia' 'Marshall University'
'Undergraduate Schools'  'West Virginia' 'Shepherd University'
'Undergraduate Schools'  'West Virginia' 'University of Charleston'
'Undergraduate Schools'  'West Virginia' 'West Virginia University'
'Undergraduate Schools'  'Wisconsin' 'Beloit College'
'Undergraduate Schools'  'Wisconsin' 'Cardinal Stritch University'
'Undergraduate Schools'  'Wisconsin' 'Concordia University'
'Undergraduate Schools'  'Wisconsin' 'Edgewood College'
'Undergraduate Schools'  'Wisconsin' 'Lawrence University'
'Undergraduate Schools'  'Wisconsin' 'Marquette University'
'Undergraduate Schools'  'Wisconsin' 'Ripon College'
'Undergraduate Schools'  'Wisconsin' 'St. Norbert College'
'Undergraduate Schools'  'Wisconsin' 'University of Wisconsin - Madison'
'Undergraduate Schools'  'Wisconsin' 'University of Wisconsin - Milwaukee'
'Undergraduate Schools'  'Wisconsin' 'University of Wisconsin - Parkside'
'Undergraduate Schools'  'Wisconsin' 'University of Wisconsin - Superior'
'Undergraduate Schools'  'Wyoming' 'University of Wyoming'

",
header = TRUE, stringsAsFactors = FALSE)

  output$school_selector = renderUI({
    selectInput(inputId = "school",
                label = em(h4("TYPE OF SCHOOL:")),
                choices = as.character(unique(countyData$School)))
  })

  output$state_selector = renderUI({
    selectInput(inputId = "state",
                label = em(h4("STATE:")),
                choices = as.character(unique(countyData$State)))
  })

  output$county_selector = renderUI({
    data_available2 <- countyData[countyData$School == input$school & countyData$State == input$state, "University"]
    selectInput(inputId = "county",
                label = em(h4("SCHOOLS:")),
                choices = unique(data_available2),
                selected = unique(data_available2)[1])
  })



  output$TimeModel <- renderPlot({
    req(subscriber())
    data <- dataDFM()
    timeModel(data)
  })


  output$universitymostUsedSingleWordOutput <- renderPlot({
      req(subscriber())
      data <-getDataSet(input$school, input$state, input$county)
      data<-clean_tweet_text(data)
      data<- dataDFMUniversity(data)
      mostUsedSingleWord(data)
  })

  output$universityTFIDFSingleWord <- renderPlot({
    req(subscriber())
    data <-getDataSet(input$school,input$state, input$county)
    data<-clean_tweet_text(data)
    data<- dataDFMUniversity(data)
    tfidfSingleWord(data)
  })

  output$universityBigrams <- renderPlot({
    req(subscriber())
    data <- getDataSet(input$school,input$state, input$county)
    data<-clean_tweet_text(data)
    bigrams(data)
  })

  output$universityPosAndNegWords <- renderPlot({
    req(subscriber())
    data <- getDataSet(input$school,input$state, input$county)
    data<-clean_tweet_text(data)
    data <- dataDFMUniversity(data)
    sentimentPositiveAndNegative(data)
  })


  output$universityAlternativeSentimentGraph <- renderPlot({
    req(subscriber())
    data <- getDataSet(input$school,input$state, input$county)
    data<-clean_tweet_text(data)
    alternativeSentimentHistogram(data)
  })


  output$universitySentimentHistogramAggregate <- renderPlot({
    req(subscriber())
    data <-getDataSet(input$school,input$state, input$county)
    dataFrame <- universityDataFrame(data)
    data <- sentimentData(dataFrame)
    sentimentHistogramAgSentiment(data)
  })

  output$universityTopicModel <- renderPlot({
    req(subscriber())
    data <- getDataSet(input$school,input$state, input$county)
    data<-clean_tweet_text(data)
    data <- dataDFMUniversity(data)
    data <- dataDTMUniversity(data)
    topicModel(data)
  })

  output$universityreadabilitygraph <- renderPlot({
    req(subscriber())
    data <-getDataSet(input$school,input$state, input$county)
    dataFrame<-universityReadabilityDataFrame(data)
    readAbilityGraph(dataFrame)
  })

  output$tfidfSingleWordOutput <- renderPlot({
    req(subscriber())
    data <- dataDFMEssay()
    tfidfSingleWord(data)
  })

  output$mostUsedSingleWordOutput <- renderPlot({
    req(subscriber())
    data <- dataDFMEssay()
    mostUsedSingleWord(data)
  })

  output$essayPosAndNegWords <- renderPlot({
    req(subscriber())
    data <- dataDFMEssay()
    sentimentPositiveAndNegative(data)
  })

  output$essayBigrams <- renderPlot({
    req(subscriber())
    data <- dataSentenceDataFrame()
    essayBigrams(data)
  })

  output$essayHeatMap <- renderPlot({
    req(subscriber())
    data <- dataDFMEssay()
    heatMap(data)
  })

  output$dataReadability <- renderFormattable({
    req(subscriber())
    data<-dataSentenceDataFrame()
    dataReadAbility(data)
  })

  output$readabilitygraph <- renderPlot({
    req(subscriber())
    data<-dataSentenceDataFrame()
    readAbilityGraph(data)
  })

  output$lexicalDiversity <- renderFormattable({
    req(subscriber())
    data <- dataDFMEssay()
    dataFrame <- dataSentenceDataLex()
    lexDivDataFrame(data, dataFrame)
  })


  output$lexicalDiversityBoxAndWhis <- renderPlot({
    req(subscriber())
    data <- dataDFMEssay()
    dataFrame <- dataSentenceDataLex()
    lexDivDataFrameBoxAndWhis(data, dataFrame)
  })


  output$lexicalDiversityMATTR <- renderFormattable({
    req(subscriber())
    data <- dataDFMEssay()
    dataFrame <- dataSentenceDataLex()
    lexDivDataFrameMATTR(data, dataFrame)
  })

  output$sentimentTable <- renderFormattable({
    req(subscriber())
    dataFrame <- dataSentimentSentenceDataFrame()
    data<-sentimentData(dataFrame)
    sentimentSentenceTable(data, dataFrame)
  })

  output$sentimentHistogramAggregate <- renderPlot({
    req(subscriber())
    dataFrame <- dataSentenceDataFrame()
    data <- sentimentData(dataFrame)
    sentimentHistogramAgSentiment(data)
  })

  output$alternativeSentimentGraph <- renderPlot({
    req(subscriber())
    stringData <- dataString()
    alternativeSentimentHistogram(stringData)
  })

  output$tableSentenceNumber <- renderFormattable({
    req(subscriber())
    dataFrame <- dataSentimentSentenceDataFrame()
    data<-sentimentData(dataFrame)
    sentenceTableSentenceNumber(data, dataFrame)
  })

  output$out <- renderText({
    input$response2
  })

  output$oid1 <- renderText({
    cat("As string:\n")
    cat(input$vec1)
  }

  )

  datainput <- reactive({
    validate(
      need((input$text != "") || (!is.null(input$file)),
           "Please give me some text to work upon!"
      )
    )

    if (nchar(input$text) > 0){
      words <- Corpus(VectorSource(input$text))
    }
    else if (!is.null(input$file1)){
      words <- pdf_text(input$file1$datapath)
    }

    words <- tm_map(words, stripWhitespace)
    words <- tm_map(words, content_transformer(tolower))
    words <- tm_map(words, removeWords, stopwords("SMART"))
    words <- tm_map(words, removeNumbers)
    words <- tm_map(words, removePunctuation)
    userDTM <- TermDocumentMatrix(words)
    userMatrix <- as.matrix(userDTM)
    userVector <- sort(rowSums(userMatrix), decreasing = TRUE)
    userDataFrame <- data.frame(text = names(userVector), Page=userVector)
    userDataFrame$text <- sapply(userDataFrame$text, as.character)
    userDataFrame$Page <- sapply(userDataFrame$Page, as.double)
    userDataFrame <- userDataFrame %>% select(text, Page)
  })


  heatMap <- function(dfmThing) {
    genre_simil <- textstat_simil(dfmThing, method='cosine',margin='documents')
    genre_simil_mat <- as.matrix(genre_simil)
    genre_simil_df <- as.data.frame(as.table(genre_simil_mat))
    genre_name <- unique(genre_simil_df$Var2)

    for(i in seq_along(genre_name)){
      genre_simil_df <-
        genre_simil_df[-which(genre_simil_df$Var1==genre_name[i] &
                                genre_simil_df$Var2 %in% genre_name[i:length(genre_name)]),]
    }
    genre70 <- genre_simil_df %>% group_by(Var2) %>% filter(Freq>=0.3) %>% ungroup() %>% data.frame()
    setHook(packageEvent("grDevices", "onLoad"),
            function(...) grDevices::quartz.options(width = 6, height = 6))
    ggplot(genre70,aes(x = Var2, y = Var1, fill = "#FFA319")) + geom_tile() +
      xlab("Sentences") + ylab("Sentences")
  }


  sentimentPositiveAndNegative <- function(dfmThing) {
    newDat <- tidy(dfmThing)

    tidy_books <- tibble(line = newDat$count, word = newDat$term)

    bing_word_counts <- tidy_books %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()

    bing_word_counts  <- bing_word_counts %>%
      group_by(sentiment) %>%
      top_n(35) %>%
      ungroup() %>%
      mutate(word = reorder(word, n))

    ggplot()+
      geom_bar(data=bing_word_counts, aes(x=word, y=n, fill=sentiment, color=sentiment), stat="identity")+
      xlab("Words")+
      ylab("Sentiment Frequency")+
      coord_flip()+
      facet_wrap(~sentiment, scales='free_y')+
      scale_fill_manual(values=c('#000000', '#FFA319'))+
      scale_color_manual(values=c('#FFA319', '#000000'))
  }





  topicModel <- function(data) {
    AssociatedPress<-data
    ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))

    ap_topics <- tidy(ap_lda, matrix = "beta")

    ap_top_terms <- ap_topics %>%
      group_by(topic) %>%
      top_n(30, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

    ap_top_terms<-  ap_top_terms %>%
      mutate(term = reorder_within(term, beta, topic))

    ggplot()+
      geom_bar(data=ap_top_terms, aes(x=term, y=beta, fill=factor(topic), color=factor(topic)), stat="identity")+
      xlab("Words Associated With a Category")+
      ylab("Association With Category")+
      coord_flip()+
      facet_wrap(~topic, scales='free')+
      scale_fill_manual(values=c('#000000', '#FFA319'))+
      scale_color_manual(values=c('#FFA319', '#000000')) +
      scale_x_reordered()
  }


  dataSentenceDataLex <- reactive({
    if (nchar(input$text) > 0){
      words <- as.character(input$text)
    }
    else if (!is.null(input$file1)){
      words <- pdf_text(input$file1$datapath)
    }

    exampleWords <- words
    string <- exampleWords
    string<-unlist(tokenize_sentence(string))
    stringDataFrame <- as.data.frame(string)
    stringDataFrame[] <- lapply(stringDataFrame, as.character)
    string<-quanteda::textstat_readability(stringDataFrame$string)
    stringDataFrameFinal <- data.frame(stringDataFrame$string, string$Flesch)
    colnames(stringDataFrameFinal)[1] <- "Sentence"
    colnames(stringDataFrameFinal)[2] <- "Readability"
    stringDataFrameFinal$Sentence <- as.character(stringDataFrameFinal$Sentence)
    stringDataFrameFinalFinal <- stringDataFrameFinal
  })



  bigrams <- function(dfmThing) {
    newDat <- dfmThing
    collocation<-textstat_collocations(newDat)
    collocation <-top_n(collocation, 50, z)
    ggplot(collocation, aes(x=z, y=collocation)) +
      geom_boxplot() + geom_point(shape = 21, color = "#FFA319", fill = "white",  size = 3, stroke = 3) + labs(x = "Strength of the Word Associations", y = "Word Associations")
  }

  essayBigrams <- function(dfmThing) {
    newDat <- dfmThing$Sentence
    collocation<-textstat_collocations(newDat)
    collocation <-top_n(collocation, 25, z)
    ggplot(collocation, aes(x=z, y=collocation)) +
      geom_boxplot() + geom_point(shape = 21, color = "#FFA319", fill = "white",  size = 3, stroke = 3) + labs(x = "Strength of the Word Associations", y = "Word Associations")
  }

  tfidfSingleWord <- function(dfmThing) {

    inaug_td <- tidy(dfmThing)
    inaug_tf_idf <- inaug_td %>%
      bind_tf_idf(term, document, count) %>%
      arrange(desc(tf_idf > 0))
    inaug_tf_idf<-top_n(inaug_tf_idf, 35, tf_idf)
    ggplot(inaug_tf_idf, aes(x=tf_idf, y=term)) +
      geom_boxplot() + geom_point(shape = 21, color = "#FFA319", fill = "white",  size = 3, stroke = 3) + labs(x = "Score of Unique Words", y = "Words")
  }


  mostUsedSingleWord <- function(dfmThing) {
        dfmThing %>%
        textstat_frequency(n = 35) %>%
        ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
        geom_boxplot() + geom_point(shape = 21, color = "#FFA319", fill = "white",  size = 3, stroke = 3) + labs(x = "Most Common Words", y = "Word Frequency") +
        coord_flip()
  }


  dataFrame <- reactive({
    if (nchar(input$text) > 0){
      words <- as.character(input$text)
    }
    else if (!is.null(input$file1)){
      words <- pdf_text(input$file1$datapath)

    }
    exampleWords <- words
    exampleWordsCorpus <- corpus(exampleWords) %>%
      corpus_reshape(exampleWords, to = c("sentences"), use_docvars = TRUE)
    exampleWordsCorpusDFM <- dfm(exampleWordsCorpus, remove = stopwords("english"),
                                 remove_punct = TRUE)
    exampleWordsCorpusDFM <- dfm_tolower(exampleWordsCorpusDFM)
    exampleWordsCorpusDTM <- convert(exampleWordsCorpusDFM, to = "tm")
    exampleWordsCorpusDataFrame <- tidy(exampleWordsCorpusDTM)
  })


  dataDTM <- reactive({
    if (nchar(input$text) > 0){
      words <- as.character(input$text)
    }
    else if (!is.null(input$file1)){
      words <- pdf_text(input$file1$datapath)

    }
    exampleWords <- words
    exampleWordsCorpus <- corpus(exampleWords) %>%
      corpus_reshape(exampleWords, to = c("sentences"), use_docvars = TRUE)
    exampleWordsCorpusDFM <- dfm(exampleWordsCorpus, remove = stopwords("english"),
                                 remove_punct = TRUE)
    exampleWordsCorpusDFM <- dfm_tolower(exampleWordsCorpusDFM)
    exampleWordsCorpusDTM <- convert(exampleWordsCorpusDFM, to = "tm")
  })


  dataDFMEssay <- reactive({
    if (nchar(input$text) > 0){
      words <- as.character(input$text)
    }
    else if (!is.null(input$file1)){
      words <- pdf_text(input$file1$datapath)

      #  a <- pdf_text(input$file1$datapath)
      #a <- input$file1$datapath
      #  a <- substr(a, 1, nchar(a) - 1)
      # words <- Corpus(DirSource(a))
    }
    exampleWords <- words
    exampleWordsCorpus <- corpus(exampleWords) %>%
      corpus_reshape(exampleWords, to = c("sentences"), use_docvars = TRUE)
    exampleWordsCorpusDFM <- dfm(exampleWordsCorpus, remove = stopwords("english"),
                                 remove_punct = TRUE)
    exampleWordsCorpusDFM <- dfm_tolower(exampleWordsCorpusDFM)
  })


  dataSentimentSentenceDataFrame <- reactive({
    if (nchar(input$text) > 0){
      words <- as.character(input$text)
    }
    else if (!is.null(input$file1)){
      words <- pdf_text(input$file1$datapath)
    }

    exampleWords <- words
    string <- exampleWords
    string <- gsub("(.*)","\\U\\1",string,perl=TRUE)
    string<-unlist(tokenize_sentence(string))
    stringDataFrame <- as.data.frame(string)
    stringDataFrame[] <- lapply(stringDataFrame, as.character)
    string<-quanteda::textstat_readability(stringDataFrame$string)
    stringDataFrameFinal <- data.frame(stringDataFrame$string, string$Flesch)
    colnames(stringDataFrameFinal)[1] <- "Sentence"
    colnames(stringDataFrameFinal)[2] <- "Readability"
    stringDataFrameFinal$Sentence <- as.character(stringDataFrameFinal$Sentence)
    stringDataFrameFinalFinal <- stringDataFrameFinal
  })



  dataSentenceDataFrame <- reactive({
    if (nchar(input$text) > 0){
      words <- as.character(input$text)
    }
    else if (!is.null(input$file1)){
      words <- pdf_text(input$file1$datapath)
    }

    exampleWords <- words
    string <- exampleWords
    string <- gsub("(.*)","\\U\\1",string,perl=TRUE)
    string<-unlist(tokenize_sentence(string))
    stringDataFrame <- as.data.frame(string)
    stringDataFrame[] <- lapply(stringDataFrame, as.character)
    string<-quanteda::textstat_readability(stringDataFrame$string)
    stringDataFrameFinal <- data.frame(stringDataFrame$string, string$Flesch)
    colnames(stringDataFrameFinal)[1] <- "Sentence"
    colnames(stringDataFrameFinal)[2] <- "Readability"
    stringDataFrameFinal$Sentence <- as.character(stringDataFrameFinal$Sentence)
    stringDataFrameFinalFinal <- stringDataFrameFinal
  })


  dataReadAbility <- function(dataFrame) {
    stringDataFrameFinal <- dataFrame
    formattable(stringDataFrameFinal, list(

      Readability = formatter("span",
                              style = x ~ style(color = ifelse(x < 50, "#FFA319", "#000000")))
    ))

  }

  readAbilityGraph <- function(dataFrame) {
    dataFrame <- filter(dataFrame, Readability > -100)
    stringDataFrameFinal <- dataFrame
    ggplot(stringDataFrameFinal, aes(x=Readability)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      geom_density(alpha=.2, fill="#FFA319") + xlab("Readability")+
      ylab("Density")
  }

  lexDivDataFrame <- function(dfmThing, dataFrame) {
    stringDataFrameFinalDFM <- dfmThing
    stringDataFrame <- dataFrame
    stringDFMDataframe <- convert(stringDataFrameFinalDFM, to = "data.frame")
    stringDataFrameFinalLexDiv <-textstat_lexdiv(stringDataFrameFinalDFM)
    stringDataFrameFinalLexDiv <- data.frame(stringDataFrame$Sentence, stringDataFrameFinalLexDiv$TTR)
    colnames(stringDataFrameFinalLexDiv)[1] <- "Sentence"
    colnames(stringDataFrameFinalLexDiv)[2] <- "Diversity"
    formattable(stringDataFrameFinalLexDiv, list(
      Diversity = formatter("span",
                            style = x ~ style(color = ifelse(x < 1, "#FFA319", "#000000")))
    ))
  }

  lexDivDataFrameBoxAndWhis <- function(dfmThing, dataFrame) {
    stringDataFrameFinalDFM <- dfmThing
    stringDataFrame <- dataFrame
    stringDFMDataframe <- convert(stringDataFrameFinalDFM, to = "data.frame")
    stringDataFrameFinalLexDiv <-textstat_lexdiv(stringDataFrameFinalDFM)
    stringDataFrameFinalLexDiv <- data.frame(stringDataFrame$Sentence, stringDataFrameFinalLexDiv$TTR)
    colnames(stringDataFrameFinalLexDiv)[1] <- "Sentence"
    colnames(stringDataFrameFinalLexDiv)[2] <- "Diversity"

    ggplot(stringDataFrameFinalLexDiv, aes(x = "",y=Diversity)) +
      geom_boxplot() + geom_point(shape = 21, color = "#FFA319", fill = "white",  size = 3, stroke = 3) + labs(x = "", y = "Range of Diversity") +
      geom_count() +
      scale_size_area()


  }

  lexDivDataFrameMATTR <- function(dfmThing, dataFrame) {
    stringDataFrameFinalDFM <- dfmThing
    stringDataFrame <- dataFrame
    stringDFMDataframe <- convert(stringDataFrameFinalDFM, to = "data.frame")
    stringDataFrameFinalLexDiv <-textstat_lexdiv(stringDataFrameFinalDFM)
    stringDataFrameFinalLexDiv <- data.frame(stringDataFrame$Sentence, stringDataFrameFinalLexDiv$TTR)
    colnames(stringDataFrameFinalLexDiv)[1] <- "Sentence"
    colnames(stringDataFrameFinalLexDiv)[2] <- "Diversity"
    formattable(stringDataFrameFinalLexDiv, list(
      Diversity = formatter("span",
                            style = x ~ style(color = ifelse(x < 0.9, "#FFA319", "#000000")))
    ))
  }


  sentimentData <- function(data) {
    stringDataFrame <- data
    stringDataFrame[] <- lapply(stringDataFrame, as.character)
    stringDataFrameSentiment<-sentimentr::sentiment(stringDataFrame$Sentence)
    stringDataFrameSentiment
  }

  sentimentHistogramAgSentiment <- function(data) {
    stringDataFrameSentiment<-data
    ggplot(stringDataFrameSentiment, aes(x=sentiment)) +
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      geom_density(alpha=.2, fill="#FFA319") + xlab("Sentiment")+
      ylab("Density")
  }

  sentimentSentenceTable <- function(data, dataFrame) {
    stringDataFrameSentiment<-data
    stringDataFrame <- dataFrame
    stringDataFrameFinalSentiment <- data.frame(stringDataFrame$Sentence, stringDataFrameSentiment$sentiment)
    colnames(stringDataFrameFinalSentiment)[1] <- "Sentence"
    colnames(stringDataFrameFinalSentiment)[2] <- "Sentiment"
    formattable(stringDataFrameFinalSentiment, list(
      Sentiment = formatter("span",
                            style = x ~ style(color = ifelse(x < 0, "#FFA319", "#000000")))
    ))
  }

  sentenceTableSentenceNumber <- function(data, dataFrame) {
    stringDataFrameSentiment<-data
    stringDataFrame <- dataFrame
    stringDataFrameFinalSentiment <- data.frame(stringDataFrame$Sentence, stringDataFrameSentiment$sentiment)
    colnames(stringDataFrameFinalSentiment)[1] <- "Sentence"
    colnames(stringDataFrameFinalSentiment)[2] <- "Sentiment"
    stringDataFrameFinalSentiment <- subset(stringDataFrameFinalSentiment, select = -c(Sentiment))
    stringDataFrameFinalSentiment<-dplyr::mutate(stringDataFrameFinalSentiment, "Sentence Number" = row_number())
    formattable(stringDataFrameFinalSentiment, list(
      "Sentence Number" = formatter("span",
                                    style = x ~ style(color = ifelse(x < 0, "#FFA319", "#000000")))
    ))
  }

  alternativeSentimentHistogram <- function(data) {

    nrcSentimentTable<-get_nrc_sentiment(data)
    final_df <- as.data.frame(t(nrcSentimentTable))
    emotions <- c("Anger", "Aniticpation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust", "Negative", "Positive")
    emotionDataFrame <- as.data.frame(emotions)
    emotionDataFrame <- cbind(emotionDataFrame, final_df)
    ggplot(emotionDataFrame, aes(x=V1, y=emotions)) +
      geom_boxplot() + geom_point(shape = 21, color = "#FFA319", fill = "white",  size = 3, stroke = 3) + labs(x = "Frequency of Sentiments", y = "Sentiments")

  }


  dataDTM <- reactive({
    if (nchar(input$text) > 0){
      words <- as.character(input$text)
    }
    else if (!is.null(input$file1)){
      words <- pdf_text(input$file1$datapath)
    }

    exampleWords <- words
    exampleWordsCorpus <- corpus(exampleWords) %>%
      corpus_reshape(exampleWords, to = c("sentences"), use_docvars = TRUE)
    exampleWordsCorpusDFM <- dfm(exampleWordsCorpus, remove = stopwords("english"),
                                 remove_punct = TRUE)
    exampleWordsCorpusDFM <- dfm_tolower(exampleWordsCorpusDFM)
    exampleWordsCorpusDTM <- convert(exampleWordsCorpusDFM, to = "tm")
  })

  dataDFM <- reactive({
    if (nchar(input$text) > 0){
      words <- as.character(input$text)
    }
    else if (!is.null(input$file1)){
      words <- pdf_text(input$file1$datapath)
    }

    exampleWords <- words
    exampleWordsCorpus <- corpus(exampleWords) %>%
      corpus_reshape(exampleWords, to = c("sentences"), use_docvars = TRUE)
    exampleWordsCorpusDFM <- dfm(exampleWordsCorpus, remove = stopwords("english"),
                                 remove_punct = TRUE)
    exampleWordsCorpusDFM <- dfm_tolower(exampleWordsCorpusDFM)
  })


  universityDataFrame <- function(data){
    data
  }


  pleaseSubscribe <- function() {
    ggplot() +
      theme_void() +
      geom_text(aes(0,0,label='N/A')) +
      xlab(NULL)
  }


  universityReadabilityDataFrame <- function(data){
    string<-quanteda::textstat_readability(data$Sentence)
    stringDataFrameFinal <- data.frame(string$Flesch)
    colnames(stringDataFrameFinal)[1] <- "Readability"
    stringDataFrameFinal
  }


  getDataSet <- function(schools, state, county){
    schoolsi = schools
    statei = state
    uni = county
    uniChoose <- sprintf("Schools/%s/States/%s/%s.csv",schoolsi,statei,uni)
    university2 <- drop_read_csv(paste(uniChoose))
    university2 <- university2 %>% select(text)
    university2$text <- sapply(university2$text, as.character)
    university2 <- university2 %>% select(text)
    colnames(university2)[1] <- "Sentence"
    university2
  }


  dataString <- reactive({
    if (nchar(input$text) > 0){
      words <- as.character(input$text)
    }
    else if (!is.null(input$file1)){
      words <- pdf_text(input$file1$datapath)
    }
    exampleWords <- words
    string <- exampleWords
  })


  dataDFMUniversity <- function(data){
    university2<-data
    exampleWordsCorpus <- corpus(university2) %>%
      corpus_reshape(exampleWords, to = c("sentences"), use_docvars = TRUE)
    exampleWordsCorpusDFM <- dfm(exampleWordsCorpus, remove = stopwords("english"),
                                 remove_punct = TRUE)
  }


  dataDTMUniversity <- function(dfmThing) {
    exampleWordsCorpusDTM <- convert(dfmThing, to = "tm")
    rowTotals <- slam::row_sums(exampleWordsCorpusDTM)
    exampleWordsCorpusDTM <- exampleWordsCorpusDTM[rowTotals > 0, ]
  }


  clean_tweet_text <- function(data) {
    unclean_tweet <- data$Sentence
    require(stringi)
    paste(unclean_tweet, collapse = " ")
    clean_tweet = gsub("&amp", "", unclean_tweet)
    clean_tweet = gsub("\\s*[^ /]+/[^ /]+","", clean_tweet)
    clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
    clean_tweet = gsub("@\\w+", "", clean_tweet)
    clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
    clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
    clean_tweet = gsub("http\\w+", "", clean_tweet)
    clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
    clean_tweet =  gsub("#[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+", "", clean_tweet)
    clean_tweet <- str_replace_all(clean_tweet," "," ")
    clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*","")
    clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
    clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
    clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")
    stopwords = c("http", "https", "i", "y", "a", "b", "c", "d", "e", "f", "g", "g", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w","x", "z")
    clean_tweet <- removeWords(clean_tweet, stopwords)

  }

}




