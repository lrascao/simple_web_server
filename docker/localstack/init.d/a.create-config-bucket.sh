#!/bin/bash

#
# upload to localstack S3
#
S3_PUBLIC_POLICY="
{
  \"Version\":\"2012-10-17\",
  \"Statement\":[
    {
      \"Sid\":\"AllowPublicRead\",
      \"Effect\":\"Allow\",
      \"Principal\": {
        \"AWS\": \"*\"
      },
      \"Action\": \"s3:GetObject\",
      \"Resource\":[\"arn:aws:s3:::simple-web-server/*\"]
      }
  ]
}
"
echo $S3_PUBLIC_POLICY > public-policy.json

echo "creating simple-web-server bucket"
awslocal s3 mb s3://simple-web-server
echo "done"

echo "setting a policy for the simple-web-server bucket"
awslocal s3api put-bucket-policy --bucket simple-web-server --policy file://public-policy.json
echo "done"

echo "setting an acl for the simple-web-server bucket"
awslocal s3api put-bucket-acl --bucket simple-web-server --acl public-read
echo "done"

