package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ZipUtil;

/**
 * Converts a Zip file in it's listing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.1
 */
public class ZipConverter extends AbstractFormatConverter {

	protected static Logger log = LoggerFactory.getLogger(ZipConverter.class);

	@Override
	public void internalConvert(String sid, Document document, File src, File dest) throws IOException {

		File tempFile = File.createTempFile("zipconvert", "txt");
		try (FileWriter writer = new FileWriter(tempFile);) {
			ZipUtil zipUtil = new ZipUtil();
			List<String> entries = zipUtil.listEntries(src);
			for (String line : entries) {
				writer.write(line);
				writer.write("\n");
			}
			writer.flush();

			String targetExtension = FilenameUtils.getExtension(dest.getName()).toLowerCase();
			if ("txt".equals(targetExtension)) {
				FileUtil.copyFile(tempFile, dest);
			} else if ("pdf".equals(targetExtension)) {
				FormatConverterManager manager = (FormatConverterManager) Context.get()
						.getBean(FormatConverterManager.class);
				FormatConverter converter = manager.getConverter("txt", targetExtension);

				if (converter == null)
					throw new IOException(
							String.format("Unable to find a converter from %s to %s", "txt", targetExtension));
				converter.convert(sid, document, tempFile, dest);
			}

			if (!dest.exists() || dest.length() < 1)
				throw new Exception("Empty conversion");
		} catch (Throwable e) {
			throw new IOException("Error in Zip conversion", e);
		} finally {
			if (tempFile != null)
				FileUtil.strongDelete(tempFile);
		}
	}
}