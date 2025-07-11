package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.sequence.Sequence;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.util.spring.Context;

/**
 * This servlet lists the sequences
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 8.9.4
 */
public class SequencesDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		SequenceDAO dao = Context.get(SequenceDAO.class);
		List<Sequence> sequences = dao.findByName(StringUtils.defaultString(request.getParameter("prefix")),
				session.getTenantId());
		sequences.sort((o1, o2) -> o1.getName().compareTo(o2.getName()));

		PrintWriter writer = response.getWriter();
		writer.write("<list>");
		for (Sequence sequence : sequences) {
			writer.print("<sequence>");
			writer.print("<id>" + sequence.getId() + "</id>");
			writer.print("<name><![CDATA[" + sequence.getName() + "]]></name>");
			writer.print("<value>" + sequence.getValue() + "</value>");
			writer.print("</sequence>");
		}
		writer.write("</list>");

	}
}