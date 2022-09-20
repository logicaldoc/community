package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.searchengine.IndexerTask;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for retrieving the current indexing queue
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.1
 */
public class IndexingQueueDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(IndexingQueueDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			Context context = Context.get();
			Session session = ServiceUtil.validateSession(request);
			UserDAO udao = (UserDAO) context.getBean(UserDAO.class);
			User user = udao.findById(session.getUserId());
			udao.initialize(user);

			String locale = request.getParameter("locale");
			if (StringUtils.isEmpty(locale))
				locale = user.getLanguage();

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			DocumentDAO dao = (DocumentDAO) context.getBean(DocumentDAO.class);
			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			int max = 100;
			if (StringUtils.isNotEmpty(request.getParameter("max")))
				max = Integer.parseInt(request.getParameter("max"));

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			String[] query = IndexerTask.prepareQuery();
			List<Object> records = (List<Object>) dao.findByQuery(
					"select _entity.id, _entity.customId, _entity.docRef, _entity.docRefType, _entity.lastModified, "
							+ "_entity.version, _entity.date, _entity.publisher, _entity.creation, _entity.creator, "
							+ "_entity.fileSize, _entity.fileName, _entity.color from Document _entity  where "
							+ query[0] + (StringUtils.isNotEmpty(query[1]) ? " order by " + query[1] : ""),
							(Map<String, Object>) null, max);

			/*
			 * Iterate over records composing the response XML document
			 */
			Document doc = null;
			for (Object record : records) {
				Object[] cols = (Object[]) record;

				doc = new Document();
				doc.setId((Long) cols[0]);
				doc.setCustomId((String) cols[1]);
				doc.setDocRef((Long) cols[2]);
				doc.setDocRefType((String) cols[3]);
				doc.setLastModified((Date) cols[4]);
				doc.setVersion((String) cols[5]);
				doc.setDate((Date) cols[6]);
				doc.setPublisher((String) cols[7]);
				doc.setCreation((Date) cols[8]);
				doc.setCreator((String) cols[9]);
				doc.setFileSize((Long) cols[10]);
				doc.setFileName((String) cols[11]);
				doc.setColor((String) cols[12]);

				writer.print("<document>");
				writer.print("<id>" + doc.getId() + "</id>");
				writer.print("<customId><![CDATA[" + (doc.getCustomId() != null ? doc.getCustomId() : "")
						+ "]]></customId>");
				if (doc.getDocRef() != null) {
					writer.print("<docref>" + doc.getDocRef() + "</docref>");
					if (doc.getDocRefType() != null)
						writer.print("<docrefType>" + doc.getDocRefType() + "</docrefType>");
				}

				writer.print("<icon>" + FilenameUtils.getBaseName(doc.getIcon()) + "</icon>");
				writer.print("<version>" + doc.getVersion() + "</version>");
				writer.print("<lastModified>" + (doc.getLastModified() != null ? doc.getLastModified() : "")
						+ "</lastModified>");
				writer.print("<published>" + (doc.getDate() != null ? df.format(doc.getDate()) : "") + "</published>");
				writer.print("<publisher><![CDATA[" + doc.getPublisher() + "]]></publisher>");
				writer.print(
						"<created>" + (doc.getCreation() != null ? df.format(doc.getCreation()) : "") + "</created>");
				writer.print("<creator><![CDATA[" + doc.getCreator() + "]]></creator>");
				writer.print("<size>" + doc.getFileSize() + "</size>");
				writer.print("<filename><![CDATA[" + doc.getFileName() + "]]></filename>");

				if (doc.getColor() != null)
					writer.print("<color><![CDATA[" + doc.getColor() + "]]></color>");
				writer.print("</document>");
			}

			writer.write("</list>");
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			if (e instanceof ServletException)
				throw (ServletException) e;
			else if (e instanceof IOException)
				throw (IOException) e;
			else
				throw new ServletException(e.getMessage(), e);
		}
	}
}