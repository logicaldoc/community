package com.logicaldoc.core.store;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.IOUtil;

/**
 * This class is an implementation of the Store interface to persist documents
 * in the filesystem. From the root of the documents store, this implementation
 * saves all document's files into a defined directory using the following
 * logic. The document's id is tokenized by three chars tokens, than the doc/
 * dir is appended, so if the docId=12345, the document's path will
 * be:123/45/doc.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class FSStore extends AbstractStore {

	private static final Logger log = LoggerFactory.getLogger(FSStore.class);

	public FSStore() {
		super();
	}

	@Override
	public void delete(long docId) {
		File docDir = getContainer(docId);
		if (docDir == null || !docDir.exists())
			return;

		for (File file : docDir.listFiles()) {
			log.info("Deleting stored file {}", file.getAbsolutePath());
			if (FileUtil.delete(file))
				logDeletion(docId, file.getAbsolutePath());
		}

		log.info("Deleting stored folder {}", docDir.getAbsolutePath());
		FileUtil.delete(docDir);
		logDeletion(docId, docDir.getAbsolutePath());
	}

	@Override
	public void delete(StoreResource resource) {
		File file = new File(getContainer(resource.getDocId()), resource.name());
		log.info("Deleting stored file {}", file.getAbsolutePath());
		if (FileUtil.delete(file))
			logDeletion(resource.getDocId(), file.getAbsolutePath());
	}

	/**
	 * Finds the container where all document's files are stored
	 * 
	 * @param docId The document identifier
	 * @return The document's container
	 */
	public File getContainer(long docId) {
		String relativePath = computeRelativePath(docId);
		String path = getRoot().getPath() + "/" + relativePath;
		return new File(path);
	}

	public File getRoot() {
		return new File(getDir());
	}

	@Override
	public void store(File file, StoreResource resource) throws IOException {
		checkEnabled();

		checkNotEmpty(file);

		File dir = getContainer(resource.getDocId());
		FileUtils.forceMkdir(dir);
		File dest = new File(new StringBuilder(dir.getPath()).append("/").append(resource.name()).toString());
		FileUtil.copyFile(file, dest);

		checkWriteAfterStore(resource, file.length());
	}

	@Override
	public void store(InputStream stream, StoreResource resource) throws IOException {
		File file = null;
		try {
			if (!isEnabled())
				throw new IOException("Store not enabled");

			File dir = getContainer(resource.getDocId());
			FileUtils.forceMkdir(dir);
			file = new File(new StringBuilder(dir.getPath()).append("/").append(resource.name()).toString());
			FileUtil.writeFile(stream, file.getPath());

			checkWriteAfterStore(resource, file.length());
		} catch (IOException e) {
			throw e;
		} catch (Exception e) {
			throw new IOException(e.getMessage(), e);
		} finally {
			IOUtil.close(stream);
		}
	}

	@Override
	public void writeToFile(StoreResource resource, File out) throws IOException {
		File container = getContainer(resource.getDocId());
		File file = new File(container, resource.name());
		FileUtil.copyFile(file, out);
	}

	@Override
	public InputStream getStream(long docId, String resource) throws IOException {
		File container = getContainer(docId);
		File file = new File(container, resource);

		try {
			return new BufferedInputStream(new FileInputStream(file), DEFAULT_BUFFER_SIZE);
		} catch (IOException e) {
			throw e;
		} catch (Exception e) {
			throw new IOException(e.getMessage(), e);
		}
	}

	@Override
	public long getTotalSize() {
		long size = 0;
		File docDir = getRoot();
		if (docDir.exists())
			size = FileUtils.sizeOfDirectory(docDir);
		return size;
	}

	@Override
	public byte[] getBytes(long docId, String resource, long start, long length) throws IOException {
		File container = getContainer(docId);
		File file = new File(container, resource);
		return FileUtil.toByteArray(file, start, length);
	}

	@Override
	public List<StoreResource> listResources(long docId, String fileVersion) {
		List<StoreResource> resources = new ArrayList<>();
		File container = getContainer(docId);
		File[] buf = container.listFiles((dir, name) -> {
			if (name.startsWith("."))
				return false;
			else if (StringUtils.isNotEmpty(fileVersion)) {
				return name.startsWith(fileVersion);
			}
			return true;
		});
		if (buf != null)
			for (File file : buf)
				resources.add(new StoreResource.Builder().docId(docId).name(file.getName()).build());
		return resources;
	}

	@Override
	public long size(StoreResource resource) {
		File file = getContainer(resource.getDocId());
		file = new File(file, resource.name());
		return file.length();
	}

	@Override
	public boolean exists(StoreResource resource) {
		File file = getContainer(resource.getDocId());
		file = new File(file, resource.name());
		return file.exists();
	}

	@Override
	public List<String> getParameterNames() {
		return new ArrayList<>();
	}

	@Override
	public int moveResourcesToStore(long docId, int targetStoreId) throws IOException {
		throw new UnsupportedOperationException();
	}
}