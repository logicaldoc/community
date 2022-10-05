package com.logicaldoc.webservice;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.cxf.transport.servlet.CXFServlet;

import com.logicaldoc.core.util.ServletUtil;
import com.logicaldoc.util.Context;

/**
 * Extension of the standard CXF servlet that checks the enabled flag
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.6
 */
public class WebserviceServlet extends CXFServlet {
	private static final long serialVersionUID = 1L;

	public void service(HttpServletRequest request, HttpServletResponse response) {
		try {
			// Check if the service is enabled
			if (Context.get().getProperties().getBoolean("webservice.enabled", false))
				super.service(request, response);
			else
				response.sendError(HttpServletResponse.SC_MOVED_TEMPORARILY);
		} catch (Throwable t) {
			ServletUtil.sendError(response, t.getMessage());
		}
	}
}