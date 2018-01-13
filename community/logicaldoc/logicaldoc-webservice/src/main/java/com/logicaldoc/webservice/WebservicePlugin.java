package com.logicaldoc.webservice;

import java.io.File;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.config.WebConfigurator;
import com.logicaldoc.util.plugin.LogicalDOCPlugin;

/**
 * This class provides initializations needed by this plug-in
 * 
 * @author Matteo Caruso - Logical Object
 * @since 3.6
 * 
 */
public class WebservicePlugin extends LogicalDOCPlugin {
	protected static Logger log = LoggerFactory.getLogger(WebservicePlugin.class);

	@Override
	protected void install() throws Exception {
		ContextProperties pbean = new ContextProperties();
		pbean.setProperty("webservice.mtom", "false");
		pbean.setProperty("webservice.enabled", "true");
		pbean.write();

		File dest = new File(getPluginPath());
		dest = dest.getParentFile().getParentFile();
		WebConfigurator config = new WebConfigurator(dest.getPath() + "/web.xml");
		config.addServlet("CXFServlet", WebserviceServlet.class.getName());
		config.writeXMLDoc();
		config.addServletMapping("CXFServlet", "/services/*");
		config.writeXMLDoc();
		
		setRestartRequired();
	}
}