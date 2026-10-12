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
        File file = resolveResourceFile(resource);
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
        return new File("%s/%s".formatted(getRoot().getPath(), computeRelativePath(docId)));
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
        File dest = resolveResourceFile(resource);
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
            file = resolveResourceFile(resource);
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
        File file = resolveResourceFile(resource);
        FileUtil.copyFile(file, out);
    }

    @Override
    public InputStream getStream(StoreResource resource) throws IOException {
        File file = resolveResourceFile(resource);

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
    public byte[] getBytes(StoreResource resource, long start, long length) throws IOException {
        File file = resolveResourceFile(resource);
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
                resources.add(StoreResource.builder().docId(docId).name(file.getName()).build());
        return resources;
    }

    @Override
    public long size(StoreResource resource) {
        return resolveResourceFile(resource).length();
    }

    @Override
    public boolean exists(StoreResource resource) {
        return resolveResourceFile(resource).exists();
    }

    @Override
    public List<String> getParameterNames() {
        return new ArrayList<>();
    }

    @Override
    public int moveResourcesToStore(long docId, int targetStoreId) throws IOException {
        throw new UnsupportedOperationException();
    }

    /**
     * Resolves the file of a resource, making sure it is located directly
     * inside the document's container. Protects against path traversal even if
     * the resource name was not properly sanitized.
     *
     * @param resource The resource
     * @return The file of the resource
     * @throws IllegalArgumentException if the resource would be outside the
     *         container
     */
    protected File resolveResourceFile(StoreResource resource) {
        File file = getContainer(resource.getDocId());
        return new File(file, resource.name());
    }
}