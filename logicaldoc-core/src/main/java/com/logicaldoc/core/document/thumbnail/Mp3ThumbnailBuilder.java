package com.logicaldoc.core.document.thumbnail;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

import org.apache.commons.io.FileUtils;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.FileUtil;
import com.mpatric.mp3agic.ID3v2;
import com.mpatric.mp3agic.Mp3File;

/**
 * Takes care of mp3 thumbnail builder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2
 */
public class Mp3ThumbnailBuilder extends AbstractThumbnailBuilder {

	@Override
	public synchronized void buildThumbnail(String sid, Document document, String fileVersion, File src, File dest,
			int size, int quality) throws IOException {
		try {
			Mp3File mp3file = new Mp3File(src);
			ID3v2 id3v2tag = mp3file.getId3v2Tag();
			if (id3v2tag != null) {
				String mimeType = id3v2tag.getAlbumImageMimeType();
				byte[] albumData = id3v2tag.getAlbumImage();
				String extension;
				int idx;
				if ((idx = mimeType.indexOf('/')) > 0)
					extension = "." + mimeType.substring(idx + 1).toLowerCase();
				else
					mimeType = extension = "." + mimeType.toLowerCase();

				File albumImage = FileUtil.createTempFile("album-", extension);
				try (RandomAccessFile file = new RandomAccessFile(albumImage, "rw");) {
					file.write(albumData);
					ImageThumbnailBuilder imageTBuilder = new ImageThumbnailBuilder();
					imageTBuilder.buildThumbnail(sid, document, fileVersion, albumImage, dest, size, quality);
				} finally {
					FileUtils.deleteQuietly(albumImage);
				}
			}
		} catch (Exception e) {
			throw new IOException("Error in extracting album image from the MP3", e);
		}
	}
}