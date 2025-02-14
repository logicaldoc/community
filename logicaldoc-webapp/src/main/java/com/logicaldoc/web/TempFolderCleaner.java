package com.logicaldoc.web;

import java.io.File;
import java.util.Date;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility class to clean the current Java temporary folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 */
public class TempFolderCleaner {
	private static final Logger log = LoggerFactory.getLogger(TempFolderCleaner.class);

	public void clean() {
		File tempDir = new File(System.getProperty("java.io.tmpdir"));
		log.debug("Got temporary folder {}", tempDir.getPath());

		int retentionDays = 1;
		for (File f : tempDir.listFiles()) {
			long diff = new Date().getTime() - f.lastModified();
			if (f.isFile() && !f.getName().endsWith(".data") && diff > retentionDays * 24 * 60 * 60 * 1000) {
				FileUtils.deleteQuietly(f);
				if (!f.exists())
					log.debug("Deleted file {}", f.getPath());
			}
		}
	}
}