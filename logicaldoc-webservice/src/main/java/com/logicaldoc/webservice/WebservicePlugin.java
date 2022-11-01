package com.logicaldoc.webservice;

import java.io.File;
import java.io.IOException;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.config.WebConfigurator;
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

	@Override
	public void install() throws PluginException {
		try {
			ContextProperties pbean = new ContextProperties();
			pbean.setProperty("webservice.mtom", "false");
			pbean.setProperty("webservice.enabled", "true");
			pbean.setProperty("webservice.gzip", "20");
			pbean.setProperty("webservice.call.record", "true");
			pbean.setProperty("webservice.call.ttl", "90");

			pbean.setProperty("threadpool." + WebserviceInterceptor.THREADPOOL_CALL_STORE + ".max", "20");
			pbean.setProperty("threadpool." + WebserviceInterceptor.THREADPOOL_CALL_STORE + ".type", "default");
			pbean.setProperty("threadpool." + WebserviceInterceptor.THREADPOOL_CALL_COUNTER + ".max", "20");
			pbean.setProperty("threadpool." + WebserviceInterceptor.THREADPOOL_CALL_COUNTER + ".type", "default");

			pbean.write();
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}

		File dest = new File(getPluginPath());
		dest = dest.getParentFile().getParentFile();
		WebConfigurator config = new WebConfigurator(dest.getPath() + "/web.xml");
		config.addServlet("CXFServlet", WebserviceServlet.class.getName());
		config.writeXMLDoc();
		config.addServletMapping("CXFServlet", "/services/*");
		config.writeXMLDoc();

		config.addServlet("WebserviceChart", WebserviceChartServlet.class.getName());
		config.writeXMLDoc();
		config.addServletMapping("WebserviceChart", "/webservicechart");
		config.writeXMLDoc();

		setRestartRequired();
	}
}