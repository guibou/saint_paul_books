// Replace this with the exact base URL of the library API
const TARGET_API_HOST = 'https://mediatheques-saintpaul.re"'; 

export default {
  async fetch(request, env, ctx) {
    const url = new URL(request.url);
    const targetUrlString = `https://${TARGET_API_HOST}${url.pathname}${url.search}`;
    const clientOrigin = request.headers.get('Origin') || '*';

    // 1. Instantly resolve browser CORS pre-flight requests
    if (request.method === 'OPTIONS') {
      return new Response(null, {
        headers: {
          'Access-Control-Allow-Origin': clientOrigin,
          'Access-Control-Allow-Credentials': 'true',
          'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, OPTIONS',
          'Access-Control-Allow-Headers': request.headers.get('Access-Control-Request-Headers') || '*',
          'Access-Control-Max-Age': '86400',
        },
      });
    }

    // 2. Clone headers and spoof "Same Host" parameters
    const modifiedHeaders = new Headers(request.headers);
    modifiedHeaders.set('Host', TARGET_API_HOST);
    modifiedHeaders.set('Origin', `https://${TARGET_API_HOST}`);
    modifiedHeaders.set('Referer', `https://${TARGET_API_HOST}/`);

    // 3. Handle the POST/PUT request body safely
    let requestBody = null;
    if (request.method !== 'GET' && request.method !== 'HEAD') {
      // Read the body as an ArrayBuffer to preserve exact binary data/JSON payloads
      requestBody = await request.arrayBuffer();
    }

    try {
      // 4. Execute the network query to the library backend
      const response = await fetch(targetUrlString, {
        method: request.method,
        headers: modifiedHeaders,
        body: requestBody,
        redirect: 'follow'
      });

      // 5. Build response headers and enforce CORS no matter what status code we got
      const corsHeaders = new Headers(response.headers);
      corsHeaders.set('Access-Control-Allow-Origin', clientOrigin);
      corsHeaders.set('Access-Control-Allow-Credentials', 'true');
      corsHeaders.set('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
      corsHeaders.set('Access-Control-Allow-Headers', '*');
      corsHeaders.set('Access-Control-Expose-Headers', '*');

      return new Response(response.body, {
        status: response.status,
        statusText: response.statusText,
        headers: corsHeaders,
      });

    } catch (error) {
      return new Response(JSON.stringify({ error: 'Proxy Connection Failed', message: error.message }), {
        status: 502,
        headers: { 
          'Content-Type': 'application/json', 
          'Access-Control-Allow-Origin': clientOrigin,
          'Access-Control-Allow-Credentials': 'true'
        }
      });
    }
  },
};
