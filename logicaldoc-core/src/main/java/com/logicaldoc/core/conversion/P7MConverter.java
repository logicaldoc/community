package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.P7M;

/**
 * Converter to convert image files in PDF
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class P7MConverter extends AbstractFormatConverter {

	protected static Logger log = LoggerFactory.getLogger(P7MConverter.class);

	@Override
	public void internalConvert(String sid, Document document, File src, File dest) throws IOException {
		File tmp = null;
		try {
			String baseName = FileUtil.getBaseName(document.getFileName());
			String enclosedExtension = FileUtil.getExtension(baseName).toLowerCase();
			String targetExtension = FileUtil.getExtension(dest.getName()).toLowerCase();

			tmp = FileUtil.createTempFile("p7m", "." + enclosedExtension);

			P7M p7m = new P7M(src);
			p7m.read();
			p7m.extractOriginalFile(tmp);

			FormatConverterManager manager = (FormatConverterManager) Context.get()
					.getBean(FormatConverterManager.class);
			FormatConverter converter = manager.getConverter(enclosedExtension, targetExtension);

			if (converter == null)
				throw new IOException(
						String.format("Unable to find a converter from %s to %s", enclosedExtension, targetExtension));
			converter.convert(sid, document, tmp, dest);
		} catch (Throwable e) {
			throw new IOException(e.getMessage(), e);
		} finally {
			FileUtil.strongDelete(tmp);
		}

	}
}