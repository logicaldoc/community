package com.logicaldoc.bm;

import java.io.File;
import java.util.Random;

import org.apache.commons.io.FileUtils;

import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.sun.mail.iap.ByteArray;

/**
 * Gives a random file from the docs in a source folder
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.4
 */
public class RandomFile {

	private String sourceDir = "docs";

	private Random random = new Random();
	
	private SourceFile[] sourceFiles;

	public SourceFile getSourceFile() throws Exception {
		
		if (sourceFiles == null)
			init(new File(sourceDir));
		
		int index = random.nextInt(sourceFiles.length);
		return sourceFiles[index];
	}

	public String getSourceDir() {
		return sourceDir;
	}

	public void setSourceDir(String sourceDir) {
		this.sourceDir = sourceDir;
	}

	public synchronized void init(File sourceDir) throws Exception {
		if (sourceFiles != null)
			return;

		// Ensure that the source directory is present, if specified
		if (sourceDir != null) {
			if (!sourceDir.exists()) {
				throw new Exception("The source directory to contain upload files is missing: " + sourceDir);
			}
			File[] sss  = sourceDir.listFiles();
			sourceFiles = new SourceFile[sss.length];

			ContextProperties config = Context.get().getProperties();
			boolean loadMemory = "true".equals(config.getProperty("Upload.loadinmemory"));

			
			for (int i = 0; i < sss.length; i++) {
				if (loadMemory) {	
					sourceFiles[i] = new SourceFile(sss[i], new ByteArray(FileUtils.readFileToByteArray(sss[i]), 0, (int) sss[i].length()));
				} else {
					sourceFiles[i] = new SourceFile(sss[i], null);
				}
		    }
		} else {
			sourceFiles = new SourceFile[0];
		}
	}
}
