package com.logicaldoc.core;

import java.io.IOException;
import java.util.UUID;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.dashlet.DashletContent;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.LogicalDOCPlugin;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Plugin class for the Core plugin
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5.0
 */
public class CorePlugin extends LogicalDOCPlugin {

	protected static Logger log = LoggerFactory.getLogger(CorePlugin.class);

	@Override
	protected void onStart() throws PluginException {
		try {
			ContextProperties pbean = new ContextProperties();
			if (StringUtils.isEmpty(pbean.getProperty("id"))) {
				pbean.setProperty("id", UUID.randomUUID().toString());
				pbean.write();
			}
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public void install() throws PluginException {
		try {
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
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}

		try {
			addServlet("DashletContent", DashletContent.class.getName(), "/data/dashletcontent");
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
		
		setRestartRequired();
	}
}