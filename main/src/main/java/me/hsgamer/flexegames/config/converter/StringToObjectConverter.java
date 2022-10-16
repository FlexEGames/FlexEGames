package me.hsgamer.flexegames.config.converter;

import me.hsgamer.hscore.config.annotation.converter.Converter;

import java.util.Objects;

public abstract class StringToObjectConverter<T> implements Converter {
    @Override
    public Object convert(Object o) {
        if (o == null) return null;
        return deserialize(Objects.toString(o));
    }

    @Override
    public Object convertToRaw(Object o) {
        if (o == null) return null;
        try {
            // noinspection unchecked
            return serialize((T) o);
        } catch (ClassCastException e) {
            return null;
        }
    }

    public abstract T deserialize(String rawValue);

    public abstract String serialize(T value);
}
