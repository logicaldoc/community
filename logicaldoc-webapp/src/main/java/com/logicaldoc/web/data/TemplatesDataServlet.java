package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Session;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for templates data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TemplatesDataServlet extends AbstractDataServlet {

    private static final long serialVersionUID = 1L;

    @Override
    protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
            Locale locale) throws PersistenceException, IOException {

        Long templateId = StringUtils.isNotEmpty(request.getParameter("templateId"))
                ? Long.parseLong(request.getParameter("templateId"))
                : null;
        Integer type = request.getParameter("type") != null ? Integer.parseInt(request.getParameter("type")) : null;

        TemplateDAO templateDao = TemplateDAO.get();
        Template template = templateId != null ? templateDao.findById(templateId) : null;

        PrintWriter writer = response.getWriter();
        writer.write("<list>");

        if ("true".equals(request.getParameter("withempty"))) {
            writer.print("<template>");
            writer.print("<id></id>");
            writer.print("<name> </name>");
            writer.print("<documents>0</documents>");
            writer.print("<readonly>false</readonly>");
            writer.print("</template>");
        }

        List<Template> templates = null;
        if (type != null)
            templates = templateDao.findByType(type, session.getTenantId());
        else
            templates = templateDao.findAll(session.getTenantId());

        /*
         * Iterate over the collection of templates
         */
        for (Template templ : templates) {
            if (templateDao.isReadEnable(templ.getId(), session.getUserId())
                    || (template != null && template.equals(templ))) {

                writer.print("<template>");
                writer.print(String.format("<id>%d</id>", templ.getId()));
                writer.print(String.format("<name><![CDATA[%s]]></name>", templ.getName()));
                writer.print(String.format("<label><![CDATA[%s]]></label>",
                        StringUtils.defaultIfEmpty(templ.getLabel(), templ.getName())));
                writer.print(String.format("<description><![CDATA[%s]]></description>", templ.getDescription()));
                writer.print(String.format("<readonly>%b</readonly>", templ.isReadonly()));
                writer.print(String.format("<type>%d</type>", templ.getType()));
                writer.print("</template>");
            }
        }

        writer.write("</list>");
    }
}