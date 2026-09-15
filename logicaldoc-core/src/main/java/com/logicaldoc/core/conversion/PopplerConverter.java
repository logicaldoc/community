package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.AbstractDocument;
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

    private static final String PDFTOPPM = "pdftoppm";

    private static final String PPM_ARGUMENTS = "ppmarguments";

    private static final String PDFTOTEXT = "pdftotext";

    private static final String TEXT_ARGUMENTS = "textarguments";

    private static final String PDFTOHTML = "pdftohtml";

    private static final String HTML_ARGUMENTS = "htmlarguments";

    private static final String TIMEOUT = "timeout";

    private static final String SINGLE_FILE = "-singlefile";

    @Override
    public void internalConvert(String sid, AbstractDocument document, File src, File dest) throws IOException {

        String extension = FileUtil.getExtension(dest.getName()).toLowerCase();

        try {
            List<String> commandLine = buildConversionCommand(extension);

            commandLine.add(src.getAbsolutePath());
            commandLine.add(dest.getAbsolutePath());

            executePoppler(commandLine, null);

            processConversionOutput(extension, dest);

            if (!dest.exists() || dest.length() < 1)
                throw new IOException("Empty conversion");

        } catch (IOException e) {
            throw e;
        } catch (Exception e) {
            throw new IOException("Error in PDF to image conversion", e);
        }
    }

    private void embedImagesInHtml(File htmlFile) throws IOException {

        // Read HTML
        String html = Files.readString(htmlFile.toPath());

        // Regex to find <img src="filename.png">
        Pattern pattern = Pattern.compile("src=\"([^\"]+)\"");
        Matcher matcher = pattern.matcher(html);

        StringBuffer sb = new StringBuffer();

        while (matcher.find()) {
            String filename = matcher.group(1);
            Path imgPath = Path.of(htmlFile.getParentFile().getAbsolutePath(), filename);

            if (!Files.exists(imgPath)) {
                log.error("Image not found: {}", imgPath);
                continue;
            }

            // Read image bytes
            byte[] bytes = Files.readAllBytes(imgPath);

            // Encode Base64
            String base64 = Base64.getEncoder().encodeToString(bytes);

            // Determine MIME type from extension
            String ext = filename.substring(filename.lastIndexOf('.') + 1).toLowerCase();
            String mime = switch (ext) {
                case "jpg", "jpeg" -> "image/jpeg";
                case "png" -> "image/png";
                case "gif" -> "image/gif";
                default -> "application/octet-stream";
            };

            // Replace src="file.png" with src="data:image/png;base64,...."
            String replacement = "src=\"data:" + mime + ";base64," + base64 + "\"";
            matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement));
        }

        matcher.appendTail(sb);

        // Write final HTML
        Files.writeString(htmlFile.toPath(), sb.toString());
    }

    private int getTimeout() {
        int timeout = 30;

        try {
            timeout = Integer.parseInt(getParameter(TIMEOUT));
        } catch (Exception e) {
            // Nothing to do
        }
        return timeout;
    }

    @Override
    public List<String> getParameterNames() {
        return Arrays.asList(PDFTOPPM, PPM_ARGUMENTS, PDFTOTEXT, TEXT_ARGUMENTS, PDFTOHTML, HTML_ARGUMENTS, TIMEOUT);
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

        String pdftoppm = getParameter(PDFTOPPM);

        if (StringUtils.isBlank(pdftoppm))
            throw new IOException("Poppler parameter '%s' is not configured".formatted(PDFTOPPM));

        List<String> commandLine = new ArrayList<>();
        commandLine.add(pdftoppm);

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

        if (dpi != null)
            commandLine.add("-r %d".formatted(dpi));

        if (firstPage != null)
            commandLine.add("-f %d".formatted(firstPage));

        if (lastPage != null)
            commandLine.add("-l %d".formatted(lastPage));

        if (CollectionUtils.isNotEmpty(arguments))
            commandLine.addAll(arguments);

        commandLine.add(FileUtil.quotePath(src.getAbsolutePath()));

        // Poppler wants the path prefix and appends the extension
        commandLine.add(FilenameUtils.removeExtension(dest.getAbsolutePath()));

        executePoppler(commandLine, timeout);

        File root = dest.getParentFile();

        File[] children = root.listFiles(
                (dir, name) -> !name.equals(dest.getName()) && name.startsWith(FileUtil.getBaseName(dest.getName())));

        List<File> pages = new ArrayList<>(Arrays.asList(children));

        pages = pages.stream().sorted(Comparator.comparingInt(PopplerConverter::extractPageIndex)).toList();

        if (!pages.isEmpty() && !pages.getFirst().equals(dest)) {
            FileUtil.copyFile(pages.getFirst(), dest);
        }

        if (pages.size() == 1 && !pages.getFirst().equals(dest)) {
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

    private List<String> buildConversionCommand(String extension) {
        List<String> commandLine = new ArrayList<>();

        switch (extension) {
            case "tif", "tiff":
                commandLine.add(getParameter(PDFTOPPM));
                commandLine.add(SINGLE_FILE);
                commandLine.add("-tiff");
                commandLine.add("-tiffcompression lzw");
                addConfiguredArguments(commandLine, PPM_ARGUMENTS);
                break;

            case "png":
                commandLine.add(getParameter(PDFTOPPM));
                commandLine.add(SINGLE_FILE);
                commandLine.add("-png");
                addConfiguredArguments(commandLine, PPM_ARGUMENTS);
                break;

            case "txt":
                commandLine.add(getParameter(PDFTOTEXT));
                commandLine.add(SINGLE_FILE);
                addConfiguredArguments(commandLine, TEXT_ARGUMENTS);
                break;

            case "html":
                commandLine.add(getParameter(PDFTOHTML));
                commandLine.add("-s");
                commandLine.add("-c");
                addConfiguredArguments(commandLine, HTML_ARGUMENTS);
                break;

            default:
                commandLine.add(getParameter(PDFTOPPM));
                commandLine.add(SINGLE_FILE);
                commandLine.add("-jpeg");
                addConfiguredArguments(commandLine, PPM_ARGUMENTS);
        }

        return commandLine;
    }

    private void addConfiguredArguments(List<String> commandLine, String parameterName) {
        String arguments = getParameter(parameterName);

        if (StringUtils.isNotEmpty(arguments))
            commandLine.addAll(Arrays.asList(arguments.split(" ")));
    }

    private void processConversionOutput(String extension, File destination) throws IOException {
        if (!"txt".equals(extension) && !"html".equals(extension)) {
            copyGeneratedImage(extension, destination);
        }

        if ("html".equals(extension))
            processGeneratedHtml(destination);
    }

    private void copyGeneratedImage(String extension, File destination) throws IOException {
        String outputExtension = switch (extension) {
            case "tif", "tiff" -> "tif";
            case "png" -> "png";
            default -> "jpg";
        };

        File outputFile = new File("%s.%s".formatted(destination.getAbsolutePath(), outputExtension));

        copyAndDeleteIfPresent(outputFile, destination);
    }

    private void processGeneratedHtml(File destination) throws IOException {
        File outputFile = new File(destination.getParentFile(),
                "%s-html.html".formatted(FileUtil.getBaseName(destination.getName())));

        copyAndDeleteIfPresent(outputFile, destination);

        // Embed all external images
        embedImagesInHtml(destination);
    }

    private void copyAndDeleteIfPresent(File source, File destination) throws IOException {
        if (!source.exists())
            return;
        try {
            FileUtil.copyFile(source, destination);
        } finally {
            FileUtil.delete(source);
        }
    }

    private static int extractPageIndex(File file) {
        String baseName = FilenameUtils.getBaseName(file.getName());

        int separatorIndex = baseName.lastIndexOf('-');

        return separatorIndex >= 0 ? Integer.parseInt(baseName.substring(separatorIndex + 1)) : 0;
    }
}