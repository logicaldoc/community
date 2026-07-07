package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.GroupType;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserType;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for users data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class UsersDataServlet extends AbstractDataServlet {

    private static final long serialVersionUID = 1L;

    @Override
    protected void service(
            HttpServletRequest request,
            HttpServletResponse response,
            Session session,
            Integer max,
            Locale locale) throws PersistenceException, IOException {

        String groupIdOrName = request.getParameter("groupId");
        boolean required = "true".equals(request.getParameter("required"));
        boolean skipdisabled = "true".equals(request.getParameter("skipdisabled"));

        List<User> users;
        if (StringUtils.isNotEmpty(request.getParameter("impersonifiers")))
            users = findImpersonators(Long.parseLong(request.getParameter("impersonifiers")));
        else
            users = findUsers(session, groupIdOrName);

        printUsers(users, required, skipdisabled, response);
    }

    private void printUsers(List<User> users, boolean required, boolean skipdisabled, HttpServletResponse response)
            throws IOException, PersistenceException {
        PrintWriter writer = response.getWriter();
        writer.print("<list>");
        if (!required)
            writer.print("<user><id></id><username></username><name></name></user>");

        /*
         * Iterate over records composing the response XML document
         */
        UserDAO userDao = UserDAO.get();
        for (User user : users) {
            if (user.getType() == UserType.SYSTEM || (skipdisabled && !user.isEnabled()))
                continue;

            userDao.initialize(user);

            printUser(writer, user);
        }

        writer.print("</list>");
    }

    private void printUser(PrintWriter writer, User user) throws PersistenceException {
        DateFormat df = getDateFormat();

        writer.print("<user>");
        writer.print(String.format("<id>%d</id>", user.getId()));
        writer.print(String.format("<username><![CDATA[%s]]></username>", user.getUsername()));
        writer.print(String.format("<eenabled>%b</eenabled>", user.isEnabled()));
        writer.print(String.format("<guest>%b</guest>", user.isReadonly()));
        writer.print(String.format("<name><![CDATA[%s]]></name>", StringUtils.defaultString(user.getName())));
        writer.print(
                String.format("<firstName><![CDATA[%s]]></firstName>", StringUtils.defaultString(user.getFirstName())));
        writer.print(String.format("<label><![CDATA[%s]]></label>", StringUtils.defaultString(user.getFullName())));
        writer.print(String.format("<email><![CDATA[%s]]></email>", StringUtils.defaultString(user.getEmail())));
        writer.print(String.format("<phone><![CDATA[%s]]></phone>", StringUtils.defaultString(user.getTelephone())));
        writer.print(String.format("<cell><![CDATA[%s]]></cell>", StringUtils.defaultString(user.getTelephone2())));
        writer.print(
                String.format("<whatsapp><![CDATA[%s]]></whatsapp>", StringUtils.defaultString(user.getWhatsapp())));
        writer.print(String.format("<city><![CDATA[%s]]></city>", StringUtils.defaultString(user.getCity())));
        writer.print(String.format("<department><![CDATA[%s]]></department>",
                StringUtils.defaultString(user.getDepartment())));
        writer.print(
                String.format("<building><![CDATA[%s]]></building>", StringUtils.defaultString(user.getBuilding())));
        writer.print(String.format("<organizationalUnit><![CDATA[%s]]></organizationalUnit>",
                StringUtils.defaultString(user.getOrganizationalUnit())));
        writer.print(String.format("<company><![CDATA[%s]]></company>", StringUtils.defaultString(user.getCompany())));

        writer.print(String.format("<source>%s</source>", user.getSource().name()));
        if (user.getExpire() != null)
            writer.print(String.format("<expire>%s</expire>", df.format(user.getExpire())));
        if (user.getLastLogin() != null)
            writer.print(String.format("<lastLogin>%s</lastLogin>", df.format(user.getLastLogin())));
        if (user.getCreation() != null)
            writer.print(String.format("<creation>%s</creation>", df.format(user.getCreation())));
        if (user.getUserGroup() != null)
            writer.print(String.format("<usergroup><![CDATA[%s]]></usergroup>", user.getUserGroup().getId()));

        writer.print(String.format("<groups><![CDATA[%s]]></groups>", user.getGroups().stream()
                .filter(g -> g.getType() == GroupType.DEFAULT).map(Group::getName).collect(Collectors.joining(", "))));
        writer.print(String.format("<avatar>%d</avatar>", user.getId()));
        writer.print(String.format("<sfa>%s</sfa>", StringUtils.defaultString(user.getSecondFactor())));

        if (user.getTimeZone() != null)
            writer.print(String.format("<timeZone><![CDATA[%s]]></timeZone>", user.getTimeZone()));
        writer.print("</user>");
    }

    private List<User> findImpersonators(long userId) throws PersistenceException {
        List<String> usernames = UserDAO.get().queryForList(
                "select ld_username from ld_impersonator where ld_userid = %d".formatted(userId), String.class);

        if (CollectionUtils.isEmpty(usernames)) {
            return List.of();
        } else {
            List<User> users = UserDAO.get().findByWhere(
                    "_entity.username in (%s)".formatted(
                            usernames.stream().map(u -> "'%s'".formatted(u)).collect(Collectors.joining(","))),
                    null, null);
            for (User user : users)
                UserDAO.get().initialize(user);
            return users;
        }
    }

    private List<User> findUsers(Session session, String groupIdOrName) throws PersistenceException {
        List<User> users = new ArrayList<>();

        UserDAO userDao = UserDAO.get();
        GroupDAO groupDao = GroupDAO.get();

        if (StringUtils.isNotEmpty(groupIdOrName)) {
            Group group = null;
            try {
                group = groupDao.findById(Long.parseLong(groupIdOrName));
            } catch (Exception t) {
                // Nothing to do
            }
            if (group == null)
                group = groupDao.findByName(groupIdOrName, session.getTenantId());
            groupDao.initialize(group);

            users.addAll(group.getUsers());
        } else {
            Map<String, Object> params = new HashMap<>();
            params.put("tenantId", session.getTenantId());

            users = userDao.findByWhere("_entity.tenantId = :tenantId", params, null, null);
        }
        return users;
    }
}