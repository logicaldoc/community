package com.logicaldoc.util.config;

import java.io.InputStream;
import java.net.URL;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.jdom2.Element;

/**
 * Utility class to parse a plugin descriptor
 * 
 * @author Marco Meschieri - LogicalDOC
 *
 * @since 8.7.4
 */
public class PluginDescriptorConfigurator extends XMLBean {

	public PluginDescriptorConfigurator(InputStream is) {
		super(is);
	}

	public PluginDescriptorConfigurator(String docPath) {
		super(docPath);
	}

	public PluginDescriptorConfigurator(URL docname) {
		super(docname);
	}

	public String getId() {
		return getRootElement().getAttributeValue("id");
	}

	public String getVersion() {
		return getRootElement().getAttributeValue("version");
	}

	public String getPluginClass() {
		return getRootElement().getAttributeValue("class");
	}

	public Set<String> getDependencies() {
		Set<String> deps = new HashSet<String>();
		Element requires = getRootElement().getChild("requires");
		if (requires != null) {
			@SuppressWarnings("unchecked")
			List<Element> imports = (List<Element>) requires.getChildren("import");
			for (Element imp : imports) {
				deps.add(imp.getAttributeValue("plugin-id"));
			}
		}
		return deps;
	}
}