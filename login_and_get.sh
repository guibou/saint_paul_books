set -x
user=$1
password=$2
username=$3

# Get the first sessionId and associated cookie jar
sessionId=$(curl 'https://mediatheques-saintpaul.re/iguana/www.main.cls?surl=useractivities'\
    -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:134.0) Gecko/20100101 Firefox/134.0'\
    -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'\
    -H 'Accept-Language: en-US,en;q=0.5'\
    -H 'Accept-Encoding: gzip, deflate, br, zstd' -H 'DNT: 1'\
    -H 'Sec-GPC: 1' -H 'Connection: keep-alive'\
    -H 'Upgrade-Insecure-Requests: 1'\
    -H 'Sec-Fetch-Dest: document'\
    -H 'Sec-Fetch-Mode: navigate'\
    -H 'Sec-Fetch-Site: none'\
    -H 'Sec-Fetch-User: ?1'\
    -H 'Priority: u=0, i'\
    -c cookie_jar  | pcregrep -o1 "Vfocus.Settings.sessionID = '(.*)';")
echo $sessionId

# Login
curl "https://mediatheques-saintpaul.re/iguana/Rest.Server.cls?sessionId=$sessionId&method=user/credentials"\
    -X POST\
    -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:133.0) Gecko/20100101 Firefox/133.0'\
    -H 'Accept: */*'\
    -H 'Accept-Language: en-US,en;q=0.5'\
    -H 'Accept-Encoding: gzip, deflate, br, zstd'\
    -H 'Content-Type: application/json'\
    -H 'Origin: https://mediatheques-saintpaul.re'\
    -H 'Connection: keep-alive'\
    -H 'Referer: https://mediatheques-saintpaul.re/iguana/www.main.cls?surl=useractivities' -b cookie_jar -c cookie_jar\
    -H 'Cookie: cookieControl=true; cookieControlPrefs=%5B%22preferences%22%2C%22analytics%22%2C%22marketing%22%5D'\
    -H 'Sec-Fetch-Dest: empty'\
    -H 'Sec-Fetch-Mode: cors'\
    -H 'Sec-Fetch-Site: same-origin'\
    -H 'Priority: u=0'\
    -H 'TE: trailers'\
  --data-raw "{\"request\":{\"language\":\"fre\",\"serviceProfile\":\"Iguana\",\"locationProfile\":\"\",\"user\":\"$user\",\"password\":\"$password\",\"institution\":\"\"}}" > login_infos

getSessionId=$(jq -r '.response.sessionId' login_infos)
getToken=$(jq -r '.response.token' login_infos)

# Get
curl "https://mediatheques-saintpaul.re/iguana/Rest.Server.cls?sessionId=$sessionId&method=user/loans" \
  -X POST \
  -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:133.0) Gecko/20100101 Firefox/133.0' \
  -H 'Accept: */*' \
  -H 'Accept-Language: en-US,en;q=0.5' \
  -H 'Accept-Encoding: gzip, deflate, br, zstd' \
  -H 'Content-Type: application/json' \
  -H 'Origin: https://mediatheques-saintpaul.re' \
  -H 'Connection: keep-alive' \
  -H 'Referer: https://mediatheques-saintpaul.re/iguana/www.main.cls?surl=useractivities' \
  -b cookie_jar \
  -H 'Cookie: cookieControl=true; cookieControlPrefs=%5B%22preferences%22%2C%22analytics%22%2C%22marketing%22%5D' \
  -H 'Sec-Fetch-Dest: empty' \
  -H 'Sec-Fetch-Mode: cors' \
  -H 'Sec-Fetch-Site: same-origin' \
  -H 'TE: trailers' --data-raw "{\"request\":{\"sessionId\":\"$getSessionId\",\"LocationProfile\":\"\",\"range\":{\"from\":1,\"to\":10},\"sort\":{\"sortBy\":\"!\",\"sortDirection\":\"ASC\"}}}" > result_$username.json

exit 0
