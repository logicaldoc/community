package com.logicaldoc.core;

import java.io.File;
import java.util.UUID;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.dashlet.DashletContent;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.config.WebConfigurator;
import com.logicaldoc.util.plugin.LogicalDOCPlugin;

/**
 * Plugin class for the Core plugin
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5.0
 */
public class CorePlugin extends LogicalDOCPlugin {

	protected static Logger log = LoggerFactory.getLogger(CorePlugin.class);

	@Override
	protected void onStart() throws Exception {
		ContextProperties pbean = new ContextProperties();
		if (StringUtils.isEmpty(pbean.getProperty("id"))) {
			pbean.setProperty("id", UUID.randomUUID().toString());
			pbean.write();
		}
	}

	@Override
	public void install() throws Exception {
		// Enable the aspects in the runlevels
		ContextProperties pbean = new ContextProperties();
		for (String aspect : RunLevel.getAspects()) {
			for (RunLevel level : RunLevel.values())
				pbean.setProperty("aspect." + aspect + "." + level.toString(), "true");
		}

		pbean.setProperty("threadpool.Email.type", "default");
		pbean.setProperty("threadpool.EventCollector.max", "20");
		pbean.setProperty("threadpool.EventCollector.type", "default");

		pbean.write();

		// Register the needed servlets
		File dest = new File(getPluginPath());
		dest = dest.getParentFile().getParentFile();
		WebConfigurator config = new WebConfigurator(dest.getPath() + "/web.xml");
		config.addServlet("DashletContent", DashletContent.class.getName());
		config.addServletMapping("DashletContent", "/data/dashletcontent");

		setRestartRequired();
	}
}