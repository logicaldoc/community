package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.exec.Exec;

/**
 * Converter to convert image files in PDF
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class ImageConverter extends AbstractFormatConverter {

	protected static Logger log = LoggerFactory.getLogger(ImageConverter.class);

	protected static String CONVERT = "command.convert";

	@Override
	public void internalConvert(String sid, Document document, File src, File dest) throws IOException {
		String ext = FilenameUtils.getExtension(dest.getName()).toLowerCase();
		if (!"pdf".equals(ext))
			throw new IOException("Unable to convert image to " + ext);

		try {
			ContextProperties conf = Context.get().getProperties();
			String commandLine = conf.getProperty(CONVERT) + " -compress JPEG " + src.getPath() + " " + dest.getPath();
			Exec.exec(commandLine, null, null, 10);

			if (!dest.exists() || dest.length() < 1)
				throw new Exception("Empty conversion");
		} catch (Throwable e) {
			throw new IOException("Error in IMG to PDF conversion", e);
		}
	}
}