package com.logicaldoc.core.util;

import javax.servlet.http.HttpServletRequest;

/**
 * SOme utility methods useful inside a servlet
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class ServletUtil {
	
	/**
	 * Retrieves the original full URL of a request
	 * 
	 * @param request The request to inspect
	 * 
	 * @return The full URL
	 */
	public static String getFullURL(HttpServletRequest request) {
	    StringBuilder requestURL = new StringBuilder(request.getRequestURL().toString());
	    String queryString = request.getQueryString();

	    if (queryString == null) {
	        return requestURL.toString();
	    } else {
	        return requestURL.append('?').append(queryString).toString();
	    }
	}
}
