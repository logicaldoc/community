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
 * @author Marco Meschieri - Logical Objects
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
		try {
			if (docDir.exists())
				FileUtils.forceDelete(docDir);
		} catch (IOException e) {
			log.error(e.getMessage());
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
	public long store(InputStream stream, long docId, String resource) {
		if (!isEnabled()) {
			log.warn("Storer not enabled");
			return 0L;
		}

		File file = null;
		try {
			File dir = getContainer(docId);
			FileUtils.forceMkdir(dir);
			file = new File(new StringBuilder(dir.getPath()).append("/").append(resource).toString());
			FileUtil.writeFile(stream, file.getPath());
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return -1;
		} finally {
			try {
				stream.close();
			} catch (IOException e) {
			}
		}
		return file.length();
	}

	@Override
	public InputStream getStream(long docId, String resource) {
		File container = getContainer(docId);
		File file = new File(container, resource);

		try {
			return new BufferedInputStream(new FileInputStream(file), DEFAULT_BUFFER_SIZE);
		} catch (Throwable e) {
			log.error(e.getMessage());
			return null;
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
	public byte[] getBytes(long docId, String resource, long start, long length) {
		File container = getContainer(docId);
		File file = new File(container, resource);

		try {
			return FileUtil.toByteArray(file, start, length);
		} catch (Throwable e) {
			log.error(e.getMessage());
			return null;
		}
	}

	@Override
	public void delete(long docId, String resource) {
		File file = new File(getContainer(docId), resource);
		if (file.exists())
			try {
				FileUtils.forceDelete(file);
			} catch (IOException e) {
				log.error(e.getMessage());
			}
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
}