package com.logicaldoc.web.data;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeOption;
import com.logicaldoc.core.metadata.AttributeOptionDAO;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.util.csv.CSVFileWriter;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.IOUtil;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for attribute sets data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AttributeSetsDataServlet extends AbstractDataServlet {

    private static final long serialVersionUID = 1L;

    private static final Logger log = LoggerFactory.getLogger(AttributeSetsDataServlet.class);

    @Override
    protected void service(
            HttpServletRequest request,
            HttpServletResponse response,
            Session session,
            Integer max,
            Locale locale) throws PersistenceException, IOException {

        response.setCharacterEncoding("UTF-8");
        
        Long export = null;
        if (request.getParameter("export") != null)
            export = Long.parseLong(request.getParameter("export"));

        if (export != null) {
            // An export request
            if (!MenuDAO.get().isReadAllowed(Menu.ADMINISTRATION_METADATA, session.getUserId()))
                new PermissionException("Cannot export attribute set");
            response.setContentType("application/zip");
            export(response, export);
        } else {
            // A listing request
            response.setContentType("text/xml");
            list(request, response, session);
        }
    }

    private void export(HttpServletResponse response, long attributeSetId) throws PersistenceException, IOException {
        // Retrieve the attribute set and make a swallow copy
        AttributeSet sourceAttributeSet = AttributeSetDAO.get().findById(attributeSetId, true);
        
        log.info("Requested the export of attribute set {}", sourceAttributeSet.getName());
        
        AttributeSet swallowAttributeSet = new AttributeSet();
        BeanUtils.copyProperties(sourceAttributeSet, swallowAttributeSet, "id", "lastModified", "creation",
                "recordVersion", "attributes", "templateAttributes");
        swallowAttributeSet.setCreation(null);
        swallowAttributeSet.setLastModified(null);

        // Manually re-create the collections to avoid lazy loading cases during
        // serialization
        for (Map.Entry<String, Attribute> row : sourceAttributeSet.getAttributes().entrySet()) {
            Attribute copy = new Attribute(row.getValue());
            copy.setSetId(0L);
            swallowAttributeSet.getAttributes().put(row.getKey(), copy);
        }
        for (Map.Entry<String, Attribute> row : sourceAttributeSet.getTemplateAttributes().entrySet()) {
            Attribute copy = new Attribute(row.getValue());
            copy.setSetId(0L);
            swallowAttributeSet.getTemplateAttributes().put(row.getKey(), copy);
        }

        // Prepare the export folder and render the serialization first
        File exportRoot = FileUtil.createTempDirectory("exportattrset");

        try {
            File serialization = new File(exportRoot, "object.xml");
            FileUtil.writeFile(IOUtil.serialize(swallowAttributeSet, "UTF-8"), serialization.getAbsolutePath());

            // Export the options for any attribute as separate .csv files
            sourceAttributeSet.getTemplateAttributes().forEach((name, attribute) -> {
                if (attribute.getEditor() == Attribute.EDITOR_LISTBOX)
                    prepareOptionsCSV(name, sourceAttributeSet, exportRoot);
            });

            response.setHeader("Content-Disposition",
                    "attachment; filename=\"attributeset-%s.zip\"".formatted(sourceAttributeSet.getName()));

            // Serve the zip of the export folder
            serveFinalZip(response, exportRoot);
        } finally {
            FileUtil.delete(exportRoot);
        }
    }

    private void serveFinalZip(HttpServletResponse response, File exportRoot) throws IOException {
        try (ZipOutputStream zos = new ZipOutputStream(response.getOutputStream())) {
            Path source = exportRoot.toPath();
            Files.walk(source).filter(Files::isRegularFile).forEach(file -> {
                try {
                    zos.putNextEntry(new ZipEntry(source.relativize(file).toString()));
                    Files.copy(file, zos);
                    zos.closeEntry();
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
            });
        }
    }

    private void prepareOptionsCSV(String attributeName, AttributeSet attributeSet, File exportRoot) {
        AttributeOptionDAO optionsDao = AttributeOptionDAO.get();
        try {
            File csv = new File(exportRoot, "%s.csv".formatted(attributeName));
            try (CSVFileWriter csvWriter = new CSVFileWriter(csv.getAbsolutePath())) {
                csvWriter.writeFields(List.of("Value", "Category"));
                List<AttributeOption> options = optionsDao.findByAttribute(attributeSet.getId(), attributeName);
                for (AttributeOption option : options)
                    csvWriter.writeFields(List.of(option.getValue(), StringUtils.defaultString(option.getCategory())));
            } catch (IOException e) {
                log.error(e.getMessage(), e);
            }
        } catch (PersistenceException e) {
            log.error(e.getMessage(), e);
        }
    }

    private void list(HttpServletRequest request, HttpServletResponse response, Session session)
            throws PersistenceException, IOException {
        Integer type = null;
        if (request.getParameter("type") != null)
            type = Integer.parseInt(request.getParameter("type"));

        PrintWriter writer = response.getWriter();
        writer.write("<list>");

        if ("true".equals(request.getParameter("withempty"))) {
            writer.print("<attributeset>");
            writer.print("<id></id>");
            writer.print("<name> </name>");
            writer.print("<readonly>false</readonly>");
            writer.print("</attributeset>");
        }

        AttributeSetDAO dao = AttributeSetDAO.get();
        List<AttributeSet> sets = null;
        if (type != null)
            sets = dao.findByType(type, session.getTenantId());
        else
            sets = dao.findAll(session.getTenantId());

        /*
         * Iterate over the collection of templates
         */
        for (AttributeSet set : sets) {
            writer.print("<attributeset>");
            writer.print(String.format("<id>%d</id>", set.getId()));
            writer.print(String.format("<name><![CDATA[%s]]></name>", set.getName()));
            writer.print(String.format("<label><![CDATA[%s]]></label>",
                    StringUtils.defaultIfEmpty(set.getLabel(), set.getName())));
            writer.print(String.format("<description><![CDATA[%s]]></description>",
                    StringUtils.defaultString(set.getDescription())));
            writer.print(String.format("<readonly>%b</readonly>", set.isReadonly()));
            writer.print(String.format("<type>%d</type>", set.getType()));
            writer.print("</attributeset>");
        }

        writer.write("</list>");
    }
}