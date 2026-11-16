// From https://developers.cloudflare.com/workers/examples/cors-header-proxy/

export default {
	async fetch(request) {
		const corsHeaders = {
			"Access-Control-Allow-Origin": "*",
			"Access-Control-Allow-Methods": "GET,HEAD,POST,OPTIONS",
			"Access-Control-Max-Age": "86400",
		};

		// The URL for the remote third party API you want to fetch from
		// but does not implement CORS
		const API_URL  = "https://mediatheques-saintpaul.re/iguana/www.main.cls";
        const API_URL2 = "https://mediatheques-saintpaul.re/iguana/Rest.Server.cls"

		// The rest of this snippet for the demo page
		function rawHtmlResponse(html) {
			return new Response(html, {
				headers: {
					"content-type": "text/html;charset=UTF-8",
				},
			});
		}

		async function getSessionId()
		{
			// get the "session id" and cookies
            		let request = new Request(API_URL);
			       		let response = await fetch(request);
                let t = await response.text();
                return {
                    sessionId: t.match("Vfocus.Settings.sessionID = '(.*)'")[1],
                    cookies: response.headers.getSetCookie().join("")
                  };
		}

		async function handleRequest(request) {
			const url = new URL(request.url);
      console.log(url.origin);
			let apiUrl = url.searchParams.get("apiurl");

			if (apiUrl == null) {
				apiUrl = API_URL;
			}

			// Rewrite request to point to API URL. This also makes the request mutable
			// so you can add the correct Origin header to make the API server think
			// that this request is not cross-site.
			request = new Request(apiUrl, request);
			request.headers.set("Origin", new URL(apiUrl).origin);
      console.log(request.url);
			let response = await fetch(request);
			// Recreate the response so you can modify the headers

			response = new Response(response.body, response);
			// Set CORS headers

			response.headers.set("Access-Control-Allow-Origin", url.origin);

			// Append to/Add Vary header so browser will cache response correctly
			response.headers.append("Vary", "Origin");

			return response;
		}

		async function handleOptions(request) {
			if (
				request.headers.get("Origin") !== null &&
				request.headers.get("Access-Control-Request-Method") !== null &&
				request.headers.get("Access-Control-Request-Headers") !== null
			) {
				// Handle CORS preflight requests.
				return new Response(null, {
					headers: {
						...corsHeaders,
						"Access-Control-Allow-Headers": request.headers.get(
							"Access-Control-Request-Headers",
						),
					},
				});
			} else {
				// Handle standard OPTIONS request.
				return new Response(null, {
					headers: {
						Allow: "GET, HEAD, POST, OPTIONS",
					},
				});
			}
		}

		const url = new URL(request.url);
			if (request.method === "OPTIONS") {
				// Handle CORS preflight requests
				return handleOptions(request);
			} else if (
				request.method === "GET" ||
				request.method === "HEAD" ||
				request.method === "POST"
			) {
        if(request.method == "GET")
        {
			let id = await getSessionId();
        
                let final_response = Response.json(id);
                final_response.headers.set("Access-Control-Allow-Origin", "*");
                return final_response;
              
        }
        else if(request.method == "POST")
        {
let res = await request.json();

let url = API_URL2 + "?sessionId=" + res.sessionId + "&method=" + res.method;

let payload = res.payload;
let cookies = res.cookies;

			let request_iguana = new Request(url,
              {
                'method': 'POST',
                'body': JSON.stringify(payload),
                'headers': {
                  'Content-Type': 'application/json',
                  'Accept': '*/*',
				  'Cookie': cookies
                }
              });
			
            let response = await fetch(request_iguana);
        
            let t = await response.text();
        
			 let final_response = new Response(t);
            final_response.headers.set("Access-Control-Allow-Origin", "*");
            return final_response;
        }
				// Handle requests to the API server
				// return handleRequest(request);
			} else {
				return new Response(null, {
					status: 405,
					statusText: "Method Not Allowed",
				});
			}
	},
};
