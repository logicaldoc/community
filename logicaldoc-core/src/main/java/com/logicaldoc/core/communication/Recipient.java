package com.logicaldoc.core.communication;

import java.io.Serializable;

import org.apache.commons.lang3.StringUtils;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;

/**
 * A generic recipient of a message or email
 * 
 * @author Michael Scholz
 */
@Embeddable
public class Recipient implements Serializable {

    private static final long serialVersionUID = 1L;

    public enum Type {
        SYSTEM, EMAIL;
    }

    public enum Mode {
        TO, CC, BCC, REPLYTO, MESSAGE;
    }
    
    // The login
    @Column(name = "ld_name", nullable = false)
    private String name = "";

    // The system login or the email address
    @Column(name = "ld_address", nullable = false)
    private String address = "";

    @Column(name = "ld_mode", nullable = false)
    @Enumerated(EnumType.STRING)
    private Mode mode = Mode.TO;

    @Column(name = "ld_type", nullable = false)
    @Enumerated(EnumType.ORDINAL)
    private Type type = Type.SYSTEM;

    @Column(name = "ld_read", nullable = false)
    private int read = 0;

    public Recipient(String name, String address) {
        super();
        this.name = name;
        this.address = StringUtils.trimToEmpty(address);
    }

    public Recipient() {
    }

    public Recipient(Recipient source) {
        super();
        this.name = source.name;
        this.address = source.address;
        this.mode = source.mode;
        this.type = source.type;
        this.read = source.read;
    }

    public String getName() {
        return name;
    }

    public String getAddress() {
        return address;
    }

    public void setName(String nme) {
        name = nme;
    }

    public void setAddress(String addr) {
        address = StringUtils.trimToEmpty(addr);
    }

    @Override
    public boolean equals(Object arg0) {
        if (!(arg0 instanceof Recipient))
            return false;
        Recipient other = (Recipient) arg0;
        return other.getAddress().equals(address);
    }

    @Override
    public int hashCode() {
        return address.hashCode();
    }

    public Mode getMode() {
        return mode;
    }

    public void setMode(Mode mode) {
        this.mode = mode;
    }

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public int getRead() {
        return read;
    }

    public void setRead(int read) {
        this.read = read;
    }

    @Override
    public String toString() {
        return address;
    }
}