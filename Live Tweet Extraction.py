###Load Required Packages

import tweepy
import csv

#Insert access token 
consumer_key = #Insert key here
consumer_secret = #Insert key here
access_token = #Insert key here
access_token_secret = #Insert key here

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)

api = tweepy.API(auth)

class MyStreamListener(tweepy.StreamListener):
    #global tweetFile

    def on_status(self, status):
        try:
            with open("File Address", 'ab') as csvFile:
                tweet_id = status.id  # if type(obj.id) != None else ""
                date = status.created_at if type(status.created_at) != None else ""
                name = status.user.name.encode('utf-8') if type(status.user.name) != None else ""
                handle = status.user.screen_name if type(status.user.screen_name) != None else ""
                followers = status.user.followers_count if type(status.user.followers_count) != None else ""
                friends = status.user.friends_count if type(status.user.friends_count) != None else ""
                tweet_text = status.text.encode('utf-8') if type(status.text) != None else ""
                lang = status.user.lang if type(status.user.lang) != None else ""

                # TO get the location
                if status.place == None:
                    location = ""
                elif status.place.full_name == None:
                    location = ""
                else:
                    location = status.place.full_name

                    # TO get the co-ordinates of the location
                if status.user.geo_enabled == 'TRUE':
                    if status.geo == None:
                        coordinates = ""
                    elif status.geo['coordinates'] == None:
                        coordinates = ""
                    else:
                        coordinates = status.geo['coordinates']
                else:
                    coordinates = ""

                
                retweet = status.retweet_count if type(status.retweet_count) != None else ""

                tweet_prop = [tweet_id,date,name,handle,tweet_text,followers,friends,lang,coordinates,retweet,location]
                csv.writer(csvFile).writerow(tweet_prop)
            print('tweet saved to csv file')
        except Exception as e:
            print ('couldnt write tweet to csv file due to '+ str(e))
        print(status.user.geo_enabled)
        print(status.id)
        print(status.user.location)
        print(status.coordinates)
        print(status.geo)
        print(status.text)

    def on_error(self, status_code):
        if status_code == 420:
           
            return False

myStreamListener = MyStreamListener()
myStream = tweepy.Stream(auth = api.auth, listener = myStreamListener)
myStream.filter(track=['Keyword'], async = True)
