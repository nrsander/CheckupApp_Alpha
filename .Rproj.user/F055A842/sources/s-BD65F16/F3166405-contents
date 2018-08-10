
## Checkup_SMS


## THIS FAILS FOR SOME REASON
## ALSO DOESN'T EVEN SEND SMS WHEN IT WORKS UNLESS YOU $UB$CRIBE FOR A MEMBER$HIP
library(twilio)

# First you need to set up your accound SID and token as environmental variables
Sys.setenv(TWILIO_SID = "AC50118993ab86014854e7dc02f9142a02")
Sys.setenv(TWILIO_TOKEN = "9f5ac20d5afc846fe9bb51700acdc008")

# Then we're just going to store the numbers in some variables
my_phone_number <- "+17722148424"
twilios_phone_number <- "+16782646711"

# Now we can send away!
tw_send_message(from = twilios_phone_number, to = my_phone_number, 
                body = "Hello from R 👋")
## From: +19178675903
## To: +12125557634
## Body: Sent from your Twilio trial account - Hello from R 👋
## Status: queued
# Send a picture message
tw_send_message(twilios_phone_number, my_phone_number, 
                "Do you like the new logo?",
                media_url = "https://www.r-project.org/logo/Rlogo.png")
## From: +19178675903
## To: +12125557634
## Body: Sent from your Twilio trial account - Do you like the new logo?
## Status: queued