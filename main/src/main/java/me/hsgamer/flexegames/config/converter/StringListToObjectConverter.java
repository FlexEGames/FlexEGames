package me.hsgamer.flexegames.config.converter;

import me.hsgamer.hscore.common.CollectionUtils;
import me.hsgamer.hscore.config.annotation.converter.Converter;

import java.util.List;

public abstract class StringListToObjectConverter<T> implements Converter {
    @Override
    public Object convert(Object o) {
        if (o == null) return null;
        List<String> list = CollectionUtils.createStringListFromObject(o);
        return deserialize(list);
    }

    @Override
    public Object convertToRaw(Object o) {
        if (o == null) return null;
        try {
            // noinspection unchecked
            return serialize((T) o);
        } catch (Exception e) {
            return null;
        }
    }

    protected abstract T deserialize(List<String> rawValue);

    protected abstract List<String> serialize(T value);
}
