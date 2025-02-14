package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.exec.Exec;
import com.logicaldoc.util.io.FileUtil;

/**
 * Converter to convert image files in PDF
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class ImageConverter extends AbstractFormatConverter {

	@Override
	public void internalConvert(String sid, Document document, File src, File dest) throws IOException {
		String ext = FileUtil.getExtension(dest.getName()).toLowerCase();
		if (!"pdf".equals(ext))
			throw new IOException("Unable to convert image to " + ext);

		try {
			new Exec().exec(List.of(getParameter("path"), "-compress", "JPEG", src.getPath(), dest.getPath()), null,
					null, getTimeout());

			if (!dest.exists() || dest.length() < 1)
				throw new IOException("Empty conversion");
		} catch (IOException ioe) {
			throw ioe;
		} catch (Exception e) {
			throw new IOException("Error in IMG to PDF conversion", e);
		}
	}

	private int getTimeout() {
		int timeout = 10;
		try {
			timeout = Integer.parseInt(getParameter("timeout"));
		} catch (Exception t) {
			// Nothing to do
		}
		return timeout;
	}

	@Override
	public List<String> getParameterNames() {
		return Arrays.asList("path", "timeout");
	}
}