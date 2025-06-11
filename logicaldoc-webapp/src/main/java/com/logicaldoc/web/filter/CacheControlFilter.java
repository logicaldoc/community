package com.logicaldoc.web.filter;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.StringTokenizer;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;

/**
 * A filter that depending on the request compiles the Cache-Control header. The
 * configuration is done via the setting <code>cachecontrol</code> in the format
 * of match_pattern:value_of_cachecontrol.<br>
 * More mappings can be used separated by ;<br>
 * Eg.: cachecontrol = *.png:max-age=2592000; *.svg:max-age=2592000;
 * *.css:max-age=2592000; *.js:max-age=600; *.jsp:max-age=2592000;
 * *:max-age=2592000
 * 
 * 
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
public class CacheControlFilter implements jakarta.servlet.Filter {

	private static final Logger log = LoggerFactory.getLogger(CacheControlFilter.class);

	/**
	 * A map where key is the mapping pattern like *.js and value is the content
	 * for the Cache-Control header.
	 */
	private static Map<String, String> mappings = new LinkedHashMap<>();

	@Override
	public void destroy() {
		// Nothing
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {

		if (request instanceof HttpServletRequest httpRequest && response instanceof HttpServletResponse httpResponse) {
			if (response.isCommitted()) {
				log.debug("Request already committed {}", httpRequest.getRequestURL());
				chain.doFilter(request, response);
			} else {
				String cacheControl = getCacheControl(httpRequest.getRequestURI());
				if (log.isDebugEnabled())
					log.debug("{} -> Cache-Control: {}", httpRequest.getRequestURI(), cacheControl);
				if (cacheControl != null && !"ignore".equals(cacheControl))
					httpResponse.setHeader("Cache-Control", cacheControl);

				chain.doFilter(request, response);
			}
		} else {
			chain.doFilter(request, response);
		}
	}

	private String getCacheControl(String url) {
		for (Map.Entry<String, String> rec : mappings.entrySet()) {
			if (FileUtil.matches(url, rec.getKey(), null))
				return rec.getValue();
		}
		return null;
	}

	@Override
	public void init(FilterConfig arg0) throws ServletException {
		try {
			init(new ContextProperties().getProperty("cachecontrol", "*:no-cache,no-store,must-revalidate"));
		} catch (IOException e) {
			throw new ServletException(e.getMessage(), e);
		}
	}

	/**
	 * Initializes the configuration
	 * 
	 * @param cacheControlSpecs The full specification for the mappings. Eg.:
	 *        *.png:max-age=2592000; *.svg:max-age=2592000;
	 *        *.css:max-age=2592000; *.js:max-age=600; *.jsp:max-age=2592000;
	 *        *:max-age=2592000
	 */
	public static void init(String cacheControlSpecs) {
		StringTokenizer st = new StringTokenizer(cacheControlSpecs, ";", false);
		while (st.hasMoreElements()) {
			String token = (String) st.nextElement();
			token = token.trim();
			try {
				String[] spec = token.split(":");
				mappings.put(spec[0].trim(), spec[1].trim());
			} catch (NumberFormatException e) {
				log.warn("Cannot parse cache-control specification {}", token, e);
			}
		}
	}
}