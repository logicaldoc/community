package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.parser.ParserFactory;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for parsers data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class ParsersDataServlet extends AbstractDataServlet {

    private static final long serialVersionUID = 1L;

    @Override
    protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
            Locale locale) throws PersistenceException, IOException {

        PrintWriter writer = response.getWriter();
        writer.write("<list>");
        int i = 0;
        Set<String> keys = ParserFactory.getParsers().keySet();
        List<String> sort = new ArrayList<>();
        for (String ext : keys)
            sort.add(ext);
        Collections.sort(sort);

        for (String ext : sort) {
            writer.print("<parser>");
            writer.print(String.format("<id>%d</id>", i));
            writer.print(
                    String.format("<icon>%s</icon>", FileUtil.getBaseName(IconSelector.selectIcon(ext.toLowerCase()))));
            writer.print(String.format("<extension>%s</extension>", ext.toLowerCase()));
            writer.print(String.format("<name><![CDATA[%s]]></name>",
                    ParserFactory.getParsers().get(ext).getClass().getSimpleName()));
            writer.print(String.format("<aliases><![CDATA[%s]]></aliases>",
                    Context.get().getConfig().getProperty("parser.alias.%s".formatted(ext.toLowerCase()), "")));
            writer.print("</parser>");
        }
        writer.write("</list>");
    }
}