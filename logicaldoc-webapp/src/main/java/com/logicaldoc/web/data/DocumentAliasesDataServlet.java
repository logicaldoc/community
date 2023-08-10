package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

public class DocumentAliasesDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {
		Long docId = null;
		if (StringUtils.isNotEmpty(request.getParameter("docId")))
			docId = Long.parseLong(request.getParameter("docId"));

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		Context context = Context.get();
		DocumentDAO dao = (DocumentDAO) context.getBean(DocumentDAO.class);
		FolderDAO folderDAO = (FolderDAO) context.getBean(FolderDAO.class);
		Collection<Long> accessibleFolderIds = folderDAO.findFolderIdByUserId(session.getUserId(), null, true);

		StringBuilder query = new StringBuilder(
				"select id, fileName, folder.id from Document where deleted = 0 and docRef = " + docId);

		User user = session.getUser();

		if (!user.isMemberOf(Group.GROUP_ADMIN)) {
			if (dao.isOracle()) {
				/*
				 * In Oracle The limit of 1000 elements applies to sets of
				 * single items: (x) IN ((1), (2), (3), ...). There is no limit
				 * if the sets contain two or more items: (x, 0) IN ((1,0),
				 * (2,0), (3,0), ...):
				 */
				query.append(" and (folder.id,0) in ( ");
				boolean firstItem = true;
				for (Long folderId : accessibleFolderIds) {
					if (!firstItem)
						query.append(",");
					query.append("(");
					query.append(folderId);
					query.append(",0)");
					firstItem = false;
				}
				query.append(" )");
			} else {
				query.append(" and folder.id in ");
				query.append(accessibleFolderIds.toString().replace('[', '(').replace(']', ')'));
			}
		}

		List<Object> records = dao.findByQuery(query.toString(), (Map<String, Object>) null, null);

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Object gridRecord : records) {
			Object[] cols = (Object[]) gridRecord;
			writer.print("<alias>");
			writer.print("<id>" + cols[0] + "</id>");
			writer.print("<filename><![CDATA[" + cols[1] + "]]></filename>");
			writer.print("<folderId>" + cols[2] + "</folderId>");
			writer.print(
					"<icon>" + FileUtil.getBaseName(IconSelector.selectIcon(FileUtil.getExtension((String) cols[1])))
							+ "</icon>");
			writer.print(
					"<path><![CDATA[" + folderDAO.computePathExtended((Long) cols[2]) + "/" + cols[1] + "]]></path>");
			writer.print("</alias>");
		}
		writer.write("</list>");
	}
}