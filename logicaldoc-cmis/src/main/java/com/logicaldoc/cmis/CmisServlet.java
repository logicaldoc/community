package com.logicaldoc.cmis;

import java.io.IOException;

import org.apache.chemistry.opencmis.server.impl.atompub.CmisAtomPubServlet;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Extension of a Cmis servlet compliant with AtomPub
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5.1
 */
public class CmisServlet extends CmisAtomPubServlet {

	private static final long serialVersionUID = 1L;

	public static final ThreadLocal<String[]> remoteAddress = new ThreadLocal<>();

	@Override
	public void service(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ContextProperties settings = Context.get().getConfig();

		if ("true".equals(settings.get("cmis.enabled")))
			super.service(request, response);
		else
			response.sendError(HttpServletResponse.SC_MOVED_TEMPORARILY);
	}
}