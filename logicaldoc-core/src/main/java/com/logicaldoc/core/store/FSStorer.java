package com.logicaldoc.core.store;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.io.FileUtil;

/**
 * This class is an implementation of the Storer interface to persist documents
 * in the filesystem. From the root of the documents storage, this
 * implementation saves all document's files into a defined directory using the
 * following logic. The document's id is tokenized by three chars tokens, than
 * the doc/ dir is appended, so if the docId=12345, the document's path will
 * be:123/45/doc.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class FSStorer extends AbstractStorer {

	protected static Logger log = LoggerFactory.getLogger(FSStorer.class);

	public FSStorer() {
		super();
	}

	@Override
	public void delete(long docId) {
		File docDir = getContainer(docId);
		int maxTrials = 20;
		while (docDir.exists() && maxTrials > 0) {
			FileUtil.strongDelete(docDir);
			maxTrials--;
		}
	}

	@Override
	public void delete(long docId, String resource) {
		File file = new File(getContainer(docId), resource);
		int maxTrials = 20;
		while (file.exists() && maxTrials > 0) {
			FileUtil.strongDelete(file);
			maxTrials--;
		}
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
	public void store(File file, long docId, String resource) throws IOException {
		// Do not store 0 byte files
		if (file.length() == 0L)
			throw new IOException("Do not store 0 byte file");

		if (!isEnabled())
			throw new IOException("Storer not enabled");

		File dir = getContainer(docId);
		FileUtils.forceMkdir(dir);
		File dest = new File(new StringBuilder(dir.getPath()).append("/").append(resource).toString());
		FileUtil.copyFile(file, dest);
	}

	@Override
	public void store(InputStream stream, long docId, String resource) throws IOException {
		File file = null;
		try {
			if (!isEnabled())
				throw new IOException("Storer not enabled");

			File dir = getContainer(docId);
			FileUtils.forceMkdir(dir);
			file = new File(new StringBuilder(dir.getPath()).append("/").append(resource).toString());
			FileUtil.writeFile(stream, file.getPath());
		} catch (IOException e) {
			throw e;
		} catch (Throwable e) {
			throw new IOException(e.getMessage(), e);
		} finally {
			try {
				stream.close();
			} catch (IOException e) {
				// Nothing to do
			}
		}
	}

	@Override
	public void writeToFile(long docId, String resource, File out) throws IOException {
		File container = getContainer(docId);
		File file = new File(container, resource);
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
		} catch (Throwable e) {
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
	public List<String> listResources(long docId, final String fileVersion) {
		List<String> resources = new ArrayList<String>();
		File container = getContainer(docId);
		File[] buf = container.listFiles(new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				if (name.startsWith("."))
					return false;
				else if (StringUtils.isNotEmpty(fileVersion)) {
					return name.startsWith(fileVersion);
				}
				return true;
			}
		});
		if (buf != null)
			for (File file : buf) {
				resources.add(file.getName());
			}
		return resources;
	}

	@Override
	public long size(long docId, String resource) {
		File file = getContainer(docId);
		file = new File(file, resource);
		return file.length();
	}

	@Override
	public boolean exists(long docId, String resource) {
		File file = getContainer(docId);
		file = new File(file, resource);
		return file.exists();
	}

	@Override
	public List<String> getParameterNames() {
		return new ArrayList<String>();
	}

	@Override
	public int moveResourcesToStore(long docId, int targetStorageId) throws IOException {
		throw new UnsupportedOperationException();
	}
}