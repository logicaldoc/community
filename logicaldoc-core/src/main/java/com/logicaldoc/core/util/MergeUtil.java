package com.logicaldoc.core.util;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.apache.pdfbox.io.MemoryUsageSetting;
import org.apache.pdfbox.multipdf.PDFMergerUtility;

import com.logicaldoc.util.io.FileUtil;

/**
 * An utility to merge files
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.2
 *
 */
public class MergeUtil {

	private static final String MERGE = "merge";
	
	private MergeUtil() {
		// Not instantiable class
	}

	/**
	 * Merges different PDFs into a single PDF-
	 * 
	 * @param pdfs ordered list of pdf files to be merged
	 * @return The merged Pdf file
	 * 
	 * @throws IOException
	 */
	public static File mergePdf(List<File> pdfs) throws IOException {
		File tempDir = null;
		try {
			Path tempPath = Files.createTempDirectory(MERGE);
			tempDir = tempPath.toFile();

			File dst = FileUtil.createTempFile(MERGE, ".pdf");

			PDFMergerUtility merger = new PDFMergerUtility();
			for (File file : pdfs) {
				merger.addSource(file);
				System.out.println("Added "+file.getPath());
			}

			merger.setDestinationFileName(dst.getAbsolutePath());
			MemoryUsageSetting memoryUsage = MemoryUsageSetting.setupTempFileOnly();
			memoryUsage.setTempDir(tempDir);
			merger.mergeDocuments(memoryUsage);

			return dst;
		} finally {
			FileUtil.strongDelete(tempDir);
		}
	}
}
