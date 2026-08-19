package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.exec.Exec;
import com.logicaldoc.util.io.FileUtil;

/**
 * Converter to convert PDF into image using Poppler
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3.1
 */
public class PopplerConverter extends AbstractFormatConverter {

    private static final Logger log = LoggerFactory.getLogger(PopplerConverter.class);

    @Override
    public void internalConvert(String sid, Document document, File src, File dest) throws IOException {
        String ext = FileUtil.getExtension(dest.getName()).toLowerCase();

        try {
            List<String> commandLine = new ArrayList<>();
            String outputExt = null;
            switch (ext) {
                case "tif", "tiff":
                    outputExt = "tif";
                    commandLine.add(getParameter("pdftoppm"));
                    commandLine.add("-tiff");
                    commandLine.add("-tiffcompression lzw");
                    if (StringUtils.isNotEmpty(getParameter("ppmarguments")))
                        commandLine.addAll(Arrays.asList(getParameter("ppmarguments").split(" ")));
                    break;
                case "png":
                    outputExt = "png";
                    commandLine.add(getParameter("pdftoppm"));
                    commandLine.add("-png");
                    if (StringUtils.isNotEmpty(getParameter("ppmarguments")))
                        commandLine.addAll(Arrays.asList(getParameter("ppmarguments").split(" ")));
                    break;
                case "txt":
                    commandLine.add(getParameter("pdftotext"));
                    if (StringUtils.isNotEmpty(getParameter("textarguments")))
                        commandLine.addAll(Arrays.asList(getParameter("textarguments").split(" ")));
                    break;
                default:
                    outputExt = "jpg";
                    commandLine.add(getParameter("pdftoppm"));
                    commandLine.add("-jpeg");
                    if (StringUtils.isNotEmpty(getParameter("ppmarguments")))
                        commandLine.addAll(Arrays.asList(getParameter("ppmarguments").split(" ")));
            }

            commandLine.add("-singlefile");
            commandLine.add(src.getAbsolutePath());
            commandLine.add(dest.getAbsolutePath());

            executePoppler(commandLine, null);

            // Poppler appends file extension in case of images at the end so we
            // have to copy it into dest
            if (!"txt".equals(ext)) {
                File outputFile = new File("%s.%s".formatted(dest.getAbsolutePath(), outputExt));
                if (outputFile.exists())
                    try {
                        FileUtil.copyFile(outputFile, dest);
                    } finally {
                        FileUtil.delete(outputFile);
                    }
            }

            if (!dest.exists() || dest.length() < 1)
                throw new IOException("Empty conversion");
        } catch (IOException ioe) {
            throw ioe;
        } catch (Exception e) {
            throw new IOException("Error in PDF to image conversion", e);
        }
    }

    private int getTimeout() {
        int timeout = 30;
        try {
            timeout = Integer.parseInt(getParameter("timeout"));
        } catch (Exception t) {
            // Nothing to do
        }
        return timeout;
    }

    @Override
    public List<String> getParameterNames() {
        return Arrays.asList("pdftoppm", "ppmarguments", "pdftotext", "textarguments", "timeout");
    }

    /**
     * Prints a PDF file into an image using given resolution
     * 
     * @param src The original file
     * @param dest The output image(in case of multiple page more files are
     *        created named dest-xx.extesion)
     * @param firstPage The first page to print or null to print all the pages
     * @param lastPage The last page to print or null to print all the pages
     * @param dpi the resolution(e.g. 75, 150, 300)
     * @param arguments optional arguments
     * @param timeout maximum number of execution seconds
     * 
     * @return list of page files
     * 
     * @throws IOException Generic I/O error
     */
    public List<File> print(
            File src,
            File dest,
            Integer firstPage,
            Integer lastPage,
            Integer dpi,
            Integer timeout,
            List<String> arguments) throws IOException {
        loadParameters();

        String ext = FileUtil.getExtension(dest.getName()).toLowerCase();

        List<String> commandLine = new ArrayList<>();
        commandLine.add(getParameter("pdftoppm"));

        switch (ext) {
            case "tif", "tiff":
                commandLine.add("-tiff");
                commandLine.add("-tiffcompression lzw");
                break;
            case "png":
                commandLine.add("-png");
                break;
            default:
                commandLine.add("-jpeg");

        }

        commandLine.add("-forcenum");
        if (dpi != null)
            commandLine.add("-r %d".formatted(dpi));
        if (firstPage != null)
            commandLine.add("-f %d".formatted(firstPage));
        if (lastPage != null)
            commandLine.add("-l %d".formatted(lastPage));
        if (CollectionUtils.isNotEmpty(arguments))
            commandLine.addAll(arguments);

        commandLine.add(FileUtil.quotePath(src.getAbsolutePath()));
        commandLine.add(FileUtil.quotePath(dest.getAbsolutePath()));

        executePoppler(commandLine, timeout);

        List<File> pages = new ArrayList<>();
        File root = dest.getParentFile();
        File[] children = root.listFiles(
                (dir, name) -> !name.equals(dest.getName()) && name.startsWith(FileUtil.getBaseName(dest.getName())));
        pages.addAll(Arrays.asList(children));
        pages = pages.stream().sorted().toList();

        // Poppler appends file extension at the end so we have to copy the
        // first page to dest
        if (!pages.isEmpty())
            FileUtil.copyFile(pages.getFirst(), dest);

        // In case of single page we maintain just dest file
        if (pages.size() == 1) {
            FileUtil.delete(pages.getFirst());
            pages = List.of(dest);
        }

        return pages;
    }

    /**
     * Prints a PDF file into an image using given resolution
     * 
     * @param src The original file
     * @param dest The output image(in case of multiple page more files are
     *        created named dest-xx.extesion)
     * @param page The page to print or null to print all the pages
     * @param dpi the resolution(e.g. 75, 150, 300)
     * 
     * @return list of page files
     * 
     * @throws IOException Generic I/O error
     */
    public List<File> print(File src, File dest, Integer page, Integer dpi) throws IOException {
        return print(src, dest, page, page, dpi, null, null);
    }

    /**
     * Prints a PDF file into an image using 150dpi resolution
     * 
     * @param src The original file
     * @param dest The output image(in case of multiple page more files are
     *        created named dstName-xxx.dstExtension)
     * @param page The page to print or null to print all the pages.
     * 
     * @return list of page files
     * 
     * @throws IOException Generic I/O error
     */
    public List<File> print(File src, File dest, Integer page) throws IOException {
        return print(src, dest, page, 150);
    }

    private void executePoppler(List<String> commandLine, Integer timeout) throws IOException {
        log.debug("Executing: {}", commandLine);
        new Exec().exec(commandLine.stream().collect(Collectors.joining(" ")), null, null,
                Optional.ofNullable(timeout).orElse(getTimeout()));
    }
}