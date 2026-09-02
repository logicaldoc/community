package com.logicaldoc.util.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.file.Files;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ResourceUtil;

/**
 * A configuration utility used to retrieve and alter context properties
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class ContextProperties extends AdvancedProperties {

    public static final String LD_CONFIG = "ld.config";

    private static final String UTF_8 = "UTF-8";

    private static final String UNABLE_TO_READ_FROM = "Unable to read from %s";

    private static final long serialVersionUID = 1L;

    /** this points to an ordinary file */
    private File file;

    private File overrideFile;
    
    private AdvancedProperties overrideProperties = new AdvancedProperties();

    private static final Logger log = LoggerFactory.getLogger(ContextProperties.class);

    protected int maxBackups = 10;

    
    public ContextProperties(int maxBackups) throws IOException {
        this();
        this.maxBackups = maxBackups;
    }

    protected String getDefaultConfigLocation() {
        return StringUtils.defaultIfEmpty(System.getProperty(LD_CONFIG), "context.properties");
    }

    public ContextProperties() throws IOException {
        super();
        String config = getDefaultConfigLocation();
        if (log.isDebugEnabled())
            log.debug("Take configuration from resource {}", config);
        if (ResourceUtil.existsResource(config))
            load(ContextProperties.class.getClassLoader().getResource(config));
        else if (log.isDebugEnabled())
            log.debug("Resource {} not found", config);
    }

    public ContextProperties(String filePath) throws IOException {
        super();
        this.file = new File(filePath);

        // If the file does not exist, interpet it as a classpath reference
        if (!this.file.exists()) {
            try {
                if (filePath.startsWith("/"))
                    filePath = filePath.substring(1);
                URL url = ContextProperties.class.getClassLoader().getResource(filePath);
                if ("file".equals(url.getProtocol()))
                    this.file = new File(url.getPath());
            } catch (Exception e) {
                log.error("Unable to find classpath resource {}", filePath);
                log.error(e.getMessage(), e);
            }
        }

        try (FileInputStream fis = new FileInputStream(this.file)) {
            load(fis);
        } catch (IOException e) {
            throw new IOException(String.format(UNABLE_TO_READ_FROM, filePath), e);
        }

        overrideFile = detectOverrideFile();
        if (overrideFile != null) {
            try (FileInputStream fis = new FileInputStream(this.overrideFile)) {
                overrideProperties.load(fis);
                log.info("Override settings defined in {}", overrideFile.getPath());
            } catch (IOException e) {
                throw new IOException(String.format(UNABLE_TO_READ_FROM, overrideFile.getPath()), e);
            }
        }
    }

    protected File detectOverrideFile() {
        if (file == null || !file.exists())
            return null;
        File override = new File(file.getParentFile(), "%s-override.%s".formatted(FileUtil.getBaseName(file.getName()),
                FileUtil.getExtension(file.getName())));
        return override.exists() ? override : null;
    }

    public ContextProperties(URL fileUrl) throws IOException {
        super();
        load(fileUrl);
    }

    /**
     * Loads the file from the given URL
     */
    private void load(URL fileUrl) throws IOException {
        try {
            file = new File(URLDecoder.decode(fileUrl.getPath(), UTF_8));
        } catch (Exception e) {
            throw new IOException(String.format(UNABLE_TO_READ_FROM, file.getPath()), e);
        }
        try (FileInputStream fis = new FileInputStream(file)) {
            load(fis);
        } catch (IOException e) {
            throw new IOException(String.format(UNABLE_TO_READ_FROM, file.getPath()), e);
        }

        overrideFile = detectOverrideFile();
        if (overrideFile != null) {
            try (FileInputStream fis = new FileInputStream(overrideFile)) {
                overrideProperties.load(fis);
                log.debug("Override settings defined in {}", overrideFile.getPath());
            } catch (IOException e) {
                throw new IOException(String.format(UNABLE_TO_READ_FROM, overrideFile.getPath()), e);
            }
        }
    }

    public ContextProperties(File file) throws IOException {
        super();
        try {
            this.file = file;
            FileUtils.touch(file);
            try (FileInputStream fis = new FileInputStream(file)) {
                load(fis);
            }
        } catch (IOException e) {
            throw new IOException("Unable to read from %s".formatted(file.getPath()), e);
        }

        overrideFile = detectOverrideFile();
        if (overrideFile != null) {
            try (FileInputStream fis = new FileInputStream(overrideFile)) {
                overrideProperties.load(fis);
            } catch (IOException e) {
                throw new IOException(String.format(UNABLE_TO_READ_FROM, overrideFile.getPath()), e);
            }
        }
    }

    /**
     * Creates new XMLBean from an input stream; XMLBean is read-only!!!
     *
     * @param is the stream that represents the XML to parse
     * 
     * @throws IOException raised when the stream cannot be read
     */
    public ContextProperties(InputStream is) throws IOException {
        super();
        file = null;
        overrideFile = null;
        try {
            load(is);
        } catch (IOException e) {
            throw new IOException("Unable to read from stream", e);
        }
    }

    /**
     * This method saves the properties-file connected by ContextProperties.<br>
     * <b>NOTE:</b> only call this on an ContextProperties _NOT_ created from an
     * InputStream!
     * 
     * @throws IOException raised when the file cannot be written
     */
    public synchronized void write() throws IOException {
        checkFile();
        backup();

        File tmpFile = new File(file.getParentFile(), "%s.tmp".formatted(file.getName()));

        try {
            Files.deleteIfExists(tmpFile.toPath());
            Files.createFile(tmpFile.toPath());

            try (FileOutputStream fos = new FileOutputStream(tmpFile);) {
                store(fos, "");
                log.info("Saved settings into temp file {}", tmpFile.getAbsolutePath());
            }

            FileUtil.moveQuitely(tmpFile, file);
        } finally {
            FileUtil.delete(tmpFile);
        }
    }

    private void checkFile() throws IOException {
        // it might be that we do not have an ordinary file,
        // so we can't write to it
        if (file == null)
            throw new IOException("File not given");
    }

    /**
     * Makes a daily backup of the actual file. Up to <code>maxBackups</code>
     * are maintained.
     */
    protected void backup() throws IOException {
        if (maxBackups < 1)
            return;

        checkFile();

        // Backup the file first
        final File parent = file.getParentFile();

        /*
         * Save the daily backup
         */
        SimpleDateFormat df = new SimpleDateFormat("yyyyMMdd");
        String today = df.format(new Date());
        File backup = new File(parent, "%s.%s".formatted(file.getName(), today));
        if (!backup.exists()) {
            FileUtils.copyFile(file, backup);
            log.debug("Backup saved in {}", backup.getPath());
        }

        /*
         * Delete the oldest backups
         */
        deleteOldestBackups();
    }

    private void deleteOldestBackups() throws IOException {
        List<File> oldBackups = getBackups();
        if (oldBackups.size() > maxBackups) {
            List<File> backupsToRetain = oldBackups.stream().limit(maxBackups).toList();
            for (File backupFile : oldBackups)
                if (!backupsToRetain.contains(backupFile))
                    FileUtil.delete(backupFile);
        }
    }

    public List<File> getBackups() throws IOException {
        checkFile();

        File[] oldBackups = file.getParentFile()
                .listFiles((dir, name) -> name.startsWith("%s.".formatted(file.getName()))
                        && name.substring(name.lastIndexOf('.') + 1).length() == 8);

        // Sort old backup by descending date
        Arrays.sort(oldBackups, (f1, f2) -> {
            String date1 = f1.getName().substring(f1.getName().lastIndexOf('.') + 1);
            String date2 = f2.getName().substring(f2.getName().lastIndexOf('.') + 1);
            return date2.compareTo(date1);
        });

        return Arrays.asList(oldBackups);
    }

    public int getMaxBackups() {
        return maxBackups;
    }

    public void setMaxBackups(int maxBackups) {
        this.maxBackups = maxBackups;
    }

    @Override
    public synchronized boolean equals(Object other) {
        return super.equals(other);
    }

    @Override
    public synchronized int hashCode() {
        return super.hashCode();
    }

    @Override
    public String getProperty(String property) {
        if(overrideProperties!=null && overrideProperties.containsKey(property))
            return overrideProperties.getProperty(property);
        else
            return super.getProperty(property);
    }

    @Override
    public String getProperty(String property, String defaultValue) {
        if(overrideProperties!=null && overrideProperties.containsKey(property))
            return overrideProperties.getProperty(property, defaultValue);
        else
            return super.getProperty(property, defaultValue);
    }
}