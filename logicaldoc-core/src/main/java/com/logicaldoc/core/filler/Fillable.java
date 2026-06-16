package com.logicaldoc.core.filler;

import com.logicaldoc.core.metadata.ExtensibleObject;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;

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

    /**
     * ID of the filler to use for autofill operations
     */
    @Column(name = "ld_fillerid", nullable = true)
    protected Long fillerId;

    /**
     * Indicates id the object should be filled when created and each time it
     * gets checked-in or if it must be filled by the task Autofiller only.
     */
    @Column(name = "ld_filloncheckin", nullable = true)
    protected Boolean fillOnCheckin = true;

    public Long getFillerId() {
        return fillerId;
    }

    public void setFillerId(Long fillerId) {
        this.fillerId = fillerId;
    }

    public Boolean getFillOnCheckin() {
        return fillOnCheckin;
    }

    public void setFillOnCheckin(Boolean fillOnCheckin) {
        this.fillOnCheckin = fillOnCheckin;
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