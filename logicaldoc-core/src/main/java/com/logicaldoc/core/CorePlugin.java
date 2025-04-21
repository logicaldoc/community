package com.logicaldoc.core;

import java.io.File;
import java.io.IOException;
import java.util.UUID;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.dashlet.DashletContent;
import com.logicaldoc.core.searchengine.indexer.IndexerTask;
import com.logicaldoc.core.util.IconSelector;
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

	protected static final String DEFAULT = "default";
	
	private static final Logger log = LoggerFactory.getLogger(CorePlugin.class);

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

		// Initialize the IconSelector
		File dest = new File(getPluginPath());
		dest = dest.getParentFile().getParentFile().getParentFile();
		dest = new File(dest.getAbsolutePath() + "/frontend/sc/skins/Tahoe/images/FileIcons");
		File[] icons = dest.listFiles((File dir, String name) -> name.endsWith(".svg"));
		if (icons != null)
			for (File icon : icons)
				IconSelector.getAvailableIcons().add(FilenameUtils.getBaseName(icon.getName().toLowerCase()));
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

			pbean.setProperty("threadpool.Email.type", DEFAULT);
			pbean.setProperty("threadpool.EventCollector.max", "20");
			pbean.setProperty("threadpool.EventCollector.type", DEFAULT);
			pbean.setProperty("threadpool."+IndexerTask.NAME+".max", "2");
			pbean.setProperty("threadpool."+IndexerTask.NAME+".type", DEFAULT);

			pbean.write();
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}

		try {
			addServlet("DashletContent", DashletContent.class, "/data/dashletcontent");
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		setRestartRequired();
	}
}