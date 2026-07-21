package com.logicaldoc.core.filler;

import com.logicaldoc.core.metadata.ExtensibleObject;

import jakarta.persistence.Column;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.MappedSuperclass;
import jakarta.persistence.Transient;

/**
 * An abstract extension of {@link ExtensibleObject} that adds the necessary to
 * support autofill
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 9.2.3
 */
@MappedSuperclass
public abstract class Fillable extends ExtensibleObject {

    private static final long serialVersionUID = 1L;

    public enum FillMode {
        ALL, IMMEDIATE, DEFERRED;
    }
    
    /**
     * ID of the filler to use for autofill operations
     */
    @Column(name = "ld_fillerid", nullable = true)
    protected Long fillerId;

    @Transient
    private String fillerName;
    
    /**
     * Indicates id the object should be filled by listener, by task or both
     */
    @Column(name = "ld_fillmode", nullable = true)
    @Enumerated(EnumType.ORDINAL)
    protected FillMode fillMode = FillMode.ALL;

    public Long getFillerId() {
        return fillerId;
    }

    public void setFillerId(Long fillerId) {
        this.fillerId = fillerId;
    }

    public FillMode getFillMode() {
        return fillMode;
    }

    public void setFillMode(FillMode fillMode) {
        this.fillMode = fillMode;
    }
    
    public String getFillerName() {
        return fillerName;
    }

    public void setFillerName(String fillerName) {
        this.fillerName = fillerName;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((fillerId == null) ? 0 : fillerId.hashCode());
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
        Fillable other = (Fillable) obj;
        if (fillerId == null) {
            if (other.fillerId != null)
                return false;
        } else if (!fillerId.equals(other.fillerId))
            return false;
        return true;
    }
}