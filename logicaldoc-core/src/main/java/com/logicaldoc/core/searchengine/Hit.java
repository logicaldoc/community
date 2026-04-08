package com.logicaldoc.core.searchengine;

import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;

import com.logicaldoc.core.document.Document;

/**
 * Search result
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 5.2
 */
public class Hit extends Document implements Comparable<Hit> {

    private static final long serialVersionUID = 1L;

    private static final Logger log = LoggerFactory.getLogger(Hit.class);

    private int score;

    private String content;

    private String summary;

    public Hit() {
        // Empty constructor
    }

    public Hit(Document document) {
        try {
            BeanUtils.copyProperties(document, this, "content", "summary");
        } catch (Exception e) {
            log.warn(e.getMessage(), e);
        }
    }

    public int getScore() {
        return score;
    }

    public void setScore(int score) {
        this.score = score;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    @Override
    public int compareTo(Hit other) {
        if (this.equals(other))
            return 0;

        if (this.score > 0 && other.score > 0) {
            int cmp = Integer.compare(other.score, this.score);
            if (cmp != 0)
                return cmp;
        }

        String thisName = Objects.toString(this.getFileName(), "");
        String otherName = Objects.toString(other.getFileName(), "");
        if (thisName.equalsIgnoreCase(otherName)) {
            return Long.compare(this.getId(), other.getId());
        } else {
            return thisName.compareToIgnoreCase(otherName);
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(getClass().getName(), getId());
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!(obj instanceof Hit other))
            return false;
        return this.getId() == other.getId();
    }
}