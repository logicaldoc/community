package com.logicaldoc.core.store;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import org.apache.commons.io.FileUtils;

import com.logicaldoc.util.Context;

/**
 * This is basically a {@link FSStorer} but with a flag that if active makes the
 * store method to return an exception
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class MockStorer extends FSStorer {

	private File dummyFile = new File("pom.xml");

	private boolean errorOnStore = false;

	private boolean useDummyFile = false;

	public boolean isRaiseError() {
		return errorOnStore;
	}

	public void setErrorOnStore(boolean errorOnStore) {
		this.errorOnStore = errorOnStore;
	}

	@Override
	public void store(File file, long docId, String resource) throws IOException {
		if (errorOnStore)
			throw new IOException("error");
		if (useDummyFile)
			super.store(dummyFile, docId, resource);
		else
			super.store(file, docId, resource);
	}

	@Override
	public void store(InputStream stream, long docId, String resource) throws IOException {
		if (errorOnStore)
			throw new IOException("error");
		if (useDummyFile)
			super.store(new FileInputStream(dummyFile), docId, resource);
		else
			super.store(stream, docId, resource);
	}

	@Override
	public InputStream getStream(long docId, String resource) throws IOException {
		if (useDummyFile)
			return new FileInputStream(dummyFile);
		else
			return super.getStream(docId, resource);
	}

	public boolean isUseDummyFile() {
		return useDummyFile;
	}

	public void setUseDummyFile(boolean useDummyFile) {
		this.useDummyFile = useDummyFile;
	}

	@Override
	public int moveResourcesToStore(long docId, int targetStorageId) throws IOException {

		String targetRoot = Context.get().getProperties().getProperty("store." + targetStorageId + ".dir");

		int moved = 0;

		// List the resources
		List<String> resources = listResources(docId, null);

		for (String resource : resources) {
			File sourceFile = new File(getContainer(docId), resource);

			File targetFile = new File(targetRoot + "/" + computeRelativePath(docId) + "/" + resource);
			targetFile.getParentFile().mkdirs();

			// Extract the original file into a temporary location
			writeToFile(docId, resource, targetFile);
			moved++;

			// Delete the original resource
			FileUtils.deleteQuietly(sourceFile);
		}

		return moved;
	}

	public File getDummyFile() {
		return dummyFile;
	}

	public void setDummyFile(File dummyFile) {
		this.dummyFile = dummyFile;
	}
}