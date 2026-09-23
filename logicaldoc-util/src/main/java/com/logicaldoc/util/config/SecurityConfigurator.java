package com.logicaldoc.util.config;

import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.jdom2.Element;
import org.jdom2.Namespace;

/**
 * Configurator class for spring's context-security setup.
 * 
 * @author Marco Meschieri
 * @since 8.7.3
 */
public class SecurityConfigurator extends XMLBean {

    private static final String VALUE = "value";

    public SecurityConfigurator(String path) {
        super(path);
    }

    public SecurityConfigurator() {
        super(SecurityConfigurator.class.getClassLoader().getResource("context-security.xml"));
    }

    public String getContentSecurityPolicy() {
        Element header = getContentSecurityPolicyElement();
        if (header != null)
            return header.getAttributeValue(VALUE);
        else
            return null;
    }

    public boolean setContentSecurityPolicy(String contentSecurityPolicy) {
        Element header = getContentSecurityPolicyElement();
        boolean modified = false;
        if (header != null) {
            String currentValue = header.getAttributeValue(VALUE);
            if (currentValue != null && !currentValue.equals(contentSecurityPolicy)) {
                header.setAttribute(VALUE, contentSecurityPolicy);
                modified = true;
            }
        }
        return modified;
    }

    private Element getContentSecurityPolicyElement() {
        return findElement("//security:header[@name='Content-Security-Policy']",
                Map.of("security", "http://www.springframework.org/schema/security"));
    }

    private boolean setInterceptUrlBeforeAfter(
            String parentPattern,
            String beforeAfterPattern,
            boolean before,
            String pattern,
            String access) {
        Element http = findHttp(parentPattern);

        if (http == null)
            return false;

        List<Element> children = http.getChildren("intercept-url",
                Namespace.getNamespace("security", "http://www.springframework.org/schema/security"));
        int index = -1;
        for (Element child : children) {
            if (child.getAttributeValue("pattern").equals(pattern)) {
                // found an already existing pattern, just update it
                child.setAttribute("access", access);
                return true;
            } else if (child.getAttributeValue("pattern").equals(beforeAfterPattern)) {
                // found the pattern the new entry should be put before or after
                // of
                index = http.indexOf(child) + (before ? -1 : 1);
            }
        }

        Element interceptUrl = new Element("intercept-url",
                Namespace.getNamespace("security", "http://www.springframework.org/schema/security"));
        interceptUrl.setAttribute("pattern", pattern);
        interceptUrl.setAttribute("access", access);

        if (index >= 0)
            http.addContent(index, interceptUrl);
        else
            http.addContent(interceptUrl);
        return true;
    }

    public boolean setInterceptUrlBefore(String parentPattern, String beforePattern, String pattern, String access) {
        return setInterceptUrlBeforeAfter(parentPattern, beforePattern, true, pattern, access);
    }

    public boolean setInterceptUrlAfter(String parentPattern, String beforePattern, String pattern, String access) {
        return setInterceptUrlBeforeAfter(parentPattern, beforePattern, true, pattern, access);
    }

    public String getAccess(String parentPattern, String pattern) {
        Element http = findHttp(parentPattern);

        if (http == null)
            return null;

        List<Element> children = http.getChildren("intercept-url",
                Namespace.getNamespace("security", "http://www.springframework.org/schema/security"));
        for (Element child : children)
            if (child.getAttributeValue("pattern").equals(pattern))
                return child.getAttributeValue("access");

        return null;
    }

    private Element findHttp(String parentPattern) {
        Element http;
        if (StringUtils.isEmpty(parentPattern))
            http = findElement("//security:http[not(@pattern)]".formatted(parentPattern),
                    Map.of("security", "http://www.springframework.org/schema/security"));
        else
            http = findElement("//security:http[@pattern='%s']".formatted(parentPattern),
                    Map.of("security", "http://www.springframework.org/schema/security"));
        return http;
    }
}