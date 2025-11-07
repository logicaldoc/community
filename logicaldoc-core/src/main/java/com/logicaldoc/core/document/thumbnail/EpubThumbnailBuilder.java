package com.logicaldoc.core.document.thumbnail;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import com.github.mertakdut.NavPoint;
import com.github.mertakdut.Reader;
import com.logicaldoc.core.conversion.FormatConverter;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.conversion.NotAvailableConverter;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

/**
 * This builder generates the thumbnail for a .epub(e-book) document.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.3
 */
public class EpubThumbnailBuilder extends ImageThumbnailBuilder {

	private static final String EBOOKCOVER = "ebookcover";

	@Override
	public synchronized void buildThumbnail(String sid, Document document, String fileVersion, File src, File dest,
			int size, int compression) throws IOException {

		File tmp = null;
		try {
			Reader reader = new Reader();
			reader.setIsIncludingTextContent(true);
			reader.setFullContent(src.getAbsolutePath());

			if (reader.getCoverImage() != null) {
				/*
				 * The eBook defines a cover image, so extract and use it
				 */
				tmp = FileUtil.createTempFile(EBOOKCOVER,
						"." + (reader.getCoverImageFileName() != null
								? FileUtil.getExtension(reader.getCoverImageFileName())
								: "png"));
				FileUtil.writeFile(reader.getCoverImage(), tmp.getAbsolutePath());
				super.buildThumbnail(sid, document, fileVersion, tmp, dest, size, compression);
			} else {
				tmp = FileUtil.createTempFile(EBOOKCOVER, ".pdf");
				/*
				 * The eBook does not define a cover, so we create one using the
				 * first page
				 */
				ThumbnailBuilder pdfBuilder = Context.get(ThumbnailManager.class).getBuilder("pdf");

				FormatConverter pdfConverter = FormatConverterManager.get().getConverter("epub",
						"pdf");

				if (pdfBuilder != null && pdfConverter != null && !(pdfConverter instanceof NotAvailableConverter)) {
					/*
					 * Convert the document into a PDF and then generate it's
					 * thumbnail
					 */
					pdfConverter.convert(src, tmp);
					pdfBuilder.buildThumbnail(sid, document, fileVersion, tmp, dest, size, compression);
				} else {
					/*
					 * No PDF converter was found so write the TOC in a text
					 * file, convert it into PDF and then generate it's
					 * thumbnail
					 */
					tmp = FileUtil.createTempFile(EBOOKCOVER, ".txt");
					List<NavPoint> navPoints = reader.getToc().getNavMap().getNavPoints();
					String toc = navPoints.stream().map(p -> p.getNavLabel()).filter(StringUtils::isNotEmpty)
							.collect(Collectors.joining("\n"));
					FileUtil.writeFile(toc, tmp.getAbsolutePath());

					File pdf = FileUtil.createTempFile(EBOOKCOVER, ".pdf");
					try {
						pdfConverter = FormatConverterManager.get().getConverter("txt", "pdf");
						pdfConverter.convert(tmp, pdf);

						if (pdfBuilder != null)
							pdfBuilder.buildThumbnail(sid, document, fileVersion, pdf, dest, size, compression);
					} finally {
						FileUtil.delete(pdf);
					}
				}
			}
		} catch (Exception e) {
			throw new IOException("Thumbnail building " + e.getMessage(), e);
		} finally {
			FileUtil.delete(tmp);
		}
	}
}