package com.logicaldoc.core.document.thumbnail;

import java.io.File;
import java.io.IOException;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.store.StoreResource;
import com.logicaldoc.core.util.GhostUtil;
import com.logicaldoc.util.io.FileUtil;

/**
 * This builder generates the thumbnail for a Pdf document.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class PdfThumbnailBuilder extends ImageThumbnailBuilder {

	@Override
	public synchronized void buildThumbnail(String sid, Document document, String fileVersion, File src, File dest,
			int size, int compression) throws IOException {

		File tmp = FileUtil.createTempFile("rendertmb", StoreResource.SUFFIX_THUMBNAIL);
		try {
			GhostUtil.print(src, tmp, 1);
			super.buildThumbnail(sid, document, fileVersion, tmp, dest, size, compression);
		} catch (Exception e) {
			throw new IOException("Thumbnail building %s".formatted(e.getMessage()), e);
		} finally {
			FileUtil.delete(tmp);
		}
	}
}