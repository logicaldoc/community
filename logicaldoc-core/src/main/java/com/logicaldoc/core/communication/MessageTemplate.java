package com.logicaldoc.core.communication;

import java.util.Map;

import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.automation.AutomationException;
import com.logicaldoc.util.LocaleUtil;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

/**
 * A template for messaging purposes.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
@Entity
@Table(name = "ld_messagetemplate")
@Cacheable
public class MessageTemplate extends PersistentObject {

	private static final long serialVersionUID = 1L;

	public static final String TYPE_SYSTEM = "system";

	public static final String TYPE_USER = "user";

	@Column(name = "ld_name", length = 255, nullable = false)
	private String name = "";

	@Column(name = "ld_language", length = 10, nullable = false)
	private String language = "en";

	@Column(name = "ld_description", length = 1000)
	private String description = "";

	@Column(name = "ld_body", length = 4000)
	private String body;

	@Column(name = "ld_subject", length = 1000)
	private String subject;

	@Column(name = "ld_type", length = 255)
	private String type = TYPE_SYSTEM;

	public MessageTemplate() {
	}

	public MessageTemplate(MessageTemplate source) {
		body = source.getBody();
		description = source.getDescription();
		language = source.getLanguage();
		name = source.getName();
		subject = source.getSubject();
		setTenantId(source.getTenantId());
	}

	private String getFormattedContent(Map<String, Object> dictionary, String text) throws AutomationException {
		Automation script = new Automation(getName(), LocaleUtil.toLocale(language), getTenantId());
		String content = script.evaluate(text, dictionary);
		if (content != null)
			content = content.trim();
		return content;
	}

	public String getFormattedBody(Map<String, Object> dictionary) throws AutomationException {
		return getFormattedContent(dictionary, getBody());
	}

	public String getFormattedSubject(Map<String, Object> dictionary) throws AutomationException {
		return getFormattedContent(dictionary, getSubject());
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public String getBody() {
		return body;
	}

	public void setBody(String body) {
		this.body = body;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		MessageTemplate other = (MessageTemplate) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
}