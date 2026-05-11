package com.logicaldoc.gui.frontend.client.whatsapp;

import java.io.Serializable;

/**
 * A result for a specific Wahtsapp template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
public class TemplateResult implements Serializable {

    private static final long serialVersionUID = 1L;

    private String template;

    private String language;

    private boolean success;

    private String details;

    public TemplateResult() {
        // empty constructor
    }

    public TemplateResult(boolean success, String template, String language, String details) {
        super();
        this.template = template;
        this.language = language;
        this.success = success;
        this.details = details;
    }

    public String getTemplate() {
        return template;
    }

    public void setTemplate(String template) {
        this.template = template;
    }

    public String getLanguage() {
        return language;
    }

    public void setLanguage(String language) {
        this.language = language;
    }

    public boolean isSuccess() {
        return success;
    }

    public void setSuccess(boolean success) {
        this.success = success;
    }

    public String getDetails() {
        return details;
    }

    public void setDetails(String details) {
        this.details = details;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((details == null) ? 0 : details.hashCode());
        result = prime * result + ((language == null) ? 0 : language.hashCode());
        result = prime * result + (success ? 1231 : 1237);
        result = prime * result + ((template == null) ? 0 : template.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        TemplateResult other = (TemplateResult) obj;
        if (details == null) {
            if (other.details != null)
                return false;
        } else if (!details.equals(other.details))
            return false;
        if (language == null) {
            if (other.language != null)
                return false;
        } else if (!language.equals(other.language))
            return false;
        if (success != other.success)
            return false;
        if (template == null) {
            if (other.template != null)
                return false;
        } else if (!template.equals(other.template))
            return false;
        return true;
    }
}