import boto3
import time
import sys
import os
from boto3.dynamodb.conditions import Key, Attr

dynamodb = boto3.resource('dynamodb',
                          aws_access_key_id=os.environ['AWS_ACCESS_KEY_ID'],
                          aws_secret_access_key=os.environ['AWS_SECRET_ACCESS_KEY'],
                          region_name=os.environ['region1'])
                          
table = dynamodb.Table('tracking')
  
def q_dynamo(from_time, til):
  response = table.scan(FilterExpression=Key('time').between(from_time, til))
  data = response['Items']
  
  while 'LastEvaluatedKey' in response:
      response = table.scan(FilterExpression=Key('time_num').between(from_time, til),ExclusiveStartKey=response['LastEvaluatedKey'])
      data.extend(response['Items'])
  return data
  
  
if __name__ == "__main__":
  from_time, til = sys.argv[1:2]
  q_dynamo(from_time, til)

