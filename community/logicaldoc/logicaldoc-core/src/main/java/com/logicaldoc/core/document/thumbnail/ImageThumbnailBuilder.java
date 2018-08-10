package com.logicaldoc.core.document.thumbnail;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.exec.Exec;

/**
 * Takes care of images thumbnail builder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class ImageThumbnailBuilder extends AbstractThumbnailBuilder {
	protected static Logger log = LoggerFactory.getLogger(AbstractThumbnailBuilder.class);

	@Override
	public synchronized void buildThumbnail(String sid, Document document, String fileVersion, File src, File dest, int size, int quality)
			throws IOException {
		try {
			ContextProperties conf = Context.get().getProperties();
			String commandLine = conf.getProperty(CONVERT) + " -compress JPEG -quality " + Integer.toString(quality)
					+ " -resize x" + Integer.toString(size) + " " + src.getPath() + " " + dest.getPath();
			Exec.exec(commandLine, null, null, 10);

			if (!dest.exists() || dest.length() == 0) {
				/*
				 * In case of multiple TIF pages, the output should be
				 * name-0.jpg, name-1.jpg ...
				 */
				final String basename = FilenameUtils.getBaseName(dest.getName());
				String testname = basename + "-0.jpg";
				File test = new File(dest.getParentFile(), testname);
				if (test.exists()) {
					// In this case rename the first page with the wanted
					// destination file
					FileUtils.copyFile(test, dest);

					// And delete all other pages
					String[] pages = dest.getParentFile().list(new FilenameFilter() {
						@Override
						public boolean accept(File dir, String name) {
							return name.startsWith(basename + "-") && name.endsWith(".jpg");
						}
					});
					for (String page : pages) {
						FileUtils.deleteQuietly(new File(page));
					}
				}
			}

			if (dest.length() < 1)
				throw new Exception("Empty thumbnail image");
		} catch (Throwable e) {
			throw new IOException("Error in IMG to JPEG conversion", e);
		}
	}
}