package com.logicaldoc.webservice;

import java.io.IOException;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.LogicalDOCPlugin;
import com.logicaldoc.util.plugin.PluginException;

/**
 * This class provides initializations needed by this plug-in
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 3.6
 * 
 */
public class WebservicePlugin extends LogicalDOCPlugin {

	private static final String THREADPOOL = "threadpool.";

	@Override
	public void install() throws PluginException {
		try {
			ContextProperties pbean = new ContextProperties();
			pbean.setProperty("webservice.mtom", "false");
			pbean.setProperty("webservice.enabled", "true");
			pbean.setProperty("webservice.gzip", "20");
			pbean.setProperty("webservice.call.gridRecord", "true");
			pbean.setProperty("webservice.call.ttl", "90");
			pbean.setProperty("webservice.call.gridRecord", "true");
			pbean.setProperty("webservice.call.syncfreq", "300");
			pbean.setProperty("webservice.json.droproot", "true");

			pbean.setProperty(THREADPOOL + WebserviceInterceptor.THREADPOOL_CALL_STORE + ".max", "20");
			pbean.setProperty(THREADPOOL + WebserviceInterceptor.THREADPOOL_CALL_STORE + ".type", "default");
			pbean.setProperty(THREADPOOL + WebserviceInterceptor.THREADPOOL_CALL_COUNTER + ".max", "20");
			pbean.setProperty(THREADPOOL + WebserviceInterceptor.THREADPOOL_CALL_COUNTER + ".type", "default");

			pbean.write();
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}

		addServlet("CXFServlet", WebserviceServlet.class, "/services/*");
		addServlet("WebserviceChart", WebserviceChartServlet.class, "/webservicechart");

		addLogger("org.apache.cxf", true, "error", "WEBSERVICE");

		setRestartRequired();
	}
}