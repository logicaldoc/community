
package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;

import com.logicaldoc.core.communication.PSTWorker;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

/**
 * Converts a PST file in it's listing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.5
 */
public class PstConverter extends AbstractFormatConverter {

	@Override
	public void internalConvert(String sid, Document document, File src, File dest) throws IOException {
		PSTWorker worker = new PSTWorker(src);

		String listing = worker.printListing();

		String targetExtension = FileUtil.getExtension(dest.getName()).toLowerCase();
		if ("txt".equals(targetExtension)) {
			FileUtil.writeFile(listing, dest.getAbsolutePath());
		} else if ("pdf".equals(targetExtension)) {
			FormatConverterManager manager = Context.get(FormatConverterManager.class);
			FormatConverter converter = manager.getConverter("txt", targetExtension);

			if (converter == null)
				throw new IOException(
						String.format("Unable to find a converter from %s to %s", "txt", targetExtension));

			File tempFile = FileUtil.createTempFile("pst", ".txt");
			try {
				FileUtil.writeFile(listing, tempFile.getAbsolutePath());
				converter.convert(sid, document, tempFile, dest);
			} finally {
				FileUtil.delete(tempFile);
			}
		}

		if (!dest.exists() || dest.length() < 1)
			throw new IOException("Empty conversion");
	}
}