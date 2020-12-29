package com.logicaldoc.web;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Anti XSS vulnerability, that is the presence of &lt;script&gt; element inside
 * the request URL.
 * 
 * <br>See <a href="https://www.owasp.org/index.php/XSS">https://www.owasp.org/index.php/XSS</a>
 * <br>See <a href="https://cheatsheetseries.owasp.org/cheatsheets/Cross_Site_Scripting_Prevention_Cheat_Sheet.html">https://cheatsheetseries.owasp.org/cheatsheets/Cross_Site_Scripting_Prevention_Cheat_Sheet.html</a>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class XssFilter implements Filter {
	final Logger log = LoggerFactory.getLogger(XssFilter.class);

	public XssFilter() {
		super();
	}

	@Override
	public void init(FilterConfig config) throws ServletException {

	}

	@Override
	public void destroy() {

	}

	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		HttpServletRequest httpRequest = (HttpServletRequest) request;
		String query = httpRequest.getQueryString();
		if (StringUtils.isNotEmpty(query)) {
			boolean xssDetected = false;

			/*
			 * Apparently quicker way to do the check
			 */
			xssDetected = query.contains("%3C%2Fscript") || query.contains("%3Cscript%3E");

			/*
			 * Perhaps more robust but less efficient way
			 * 
			 * String decoded = URLDecoder.decode(query, "UTF-8"); xssDetected =
			 * decoded.contains("<script>") || decoded.contains("</script>");
			 */

			if (xssDetected) {
				String message = String.format("Detected a possible XSS attack from ip %s invoking the url %s",
						request.getRemoteAddr(), getFullURL(httpRequest));
				log.warn(message);
				throw new ServletException(message);
			}
		}

		chain.doFilter(request, response);
	}

	private String getFullURL(HttpServletRequest request) {
		StringBuilder requestURL = new StringBuilder(request.getRequestURL().toString());
		String queryString = request.getQueryString();

		if (queryString == null) {
			return requestURL.toString();
		} else {
			return requestURL.append('?').append(queryString).toString();
		}
	}
}