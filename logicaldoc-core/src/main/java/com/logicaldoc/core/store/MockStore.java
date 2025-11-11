package com.logicaldoc.core.store;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

/**
 * This is basically a {@link FSStore} but with a flag that if active makes the
 * store method to return an exception
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class MockStore extends FSStore {

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
	public void store(File file, StoreResource resource) throws IOException {
		if (errorOnStore)
			throw new IOException("error");
		if (useDummyFile)
			super.store(dummyFile, resource);
		else
			super.store(file, resource);
	}

	@Override
	public void store(InputStream stream, StoreResource resource) throws IOException {
		if (errorOnStore) {
			stream.close();
			throw new IOException("error");
		}

		if (useDummyFile) {
			stream.close();
			super.store(new FileInputStream(dummyFile), resource);
		} else {
			super.store(stream, resource);
		}
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
	public int moveResourcesToStore(long docId, int targetStoreId) throws IOException {

		String targetRoot = Context.get().getProperties().getProperty("store." + targetStoreId + ".dir");

		int moved = 0;

		// List the resources
		List<StoreResource> resources = listResources(docId, null);

		for (StoreResource resource : resources) {
			File sourceFile = new File(getContainer(resource.getDocId()), resource.name());

			File targetFile = new File(targetRoot);
			targetFile = new File(targetFile, computeRelativePath(docId));
			targetFile = new File(targetFile, resource.name());
			targetFile.getParentFile().mkdirs();

			// Extract the original file into a temporary location
			writeToFile(docId, resource.name(), targetFile);
			moved++;

			// Delete the original resource
			FileUtil.delete(sourceFile);
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