package com.logicaldoc.web.xss;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

/**
 * Anti XSS vulnerability, that is the presence of &lt;script&gt; element and other javascript code inside
 * the request URL.
 * 
 * <br>
 * See <a href=
 * "https://www.owasp.org/index.php/XSS">https://www.owasp.org/index.php/XSS</a>
 * <br>
 * See <a href=
 * "https://cheatsheetseries.owasp.org/cheatsheets/Cross_Site_Scripting_Prevention_Cheat_Sheet.html">https://cheatsheetseries.owasp.org/cheatsheets/Cross_Site_Scripting_Prevention_Cheat_Sheet.html</a>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class XssFilter implements Filter {
	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
		// Nothing to do
	}

	@Override
	public void destroy() {
		// Nothing to do
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {

		HttpServletRequest httpReq = (HttpServletRequest) request;
		if (httpReq.getServletPath().toLowerCase().endsWith("/ckeditor/index.jsp")) {
			chain.doFilter(request, response);
			return;
		}

		chain.doFilter(new XssRequestWrapper(httpReq), response);
	}
}