package me.hsgamer.epicmegagames.config.converter;

import me.hsgamer.epicmegagames.util.LoaderType;
import me.hsgamer.hscore.config.annotation.converter.Converter;

import java.util.Objects;

public class LoaderTypeConverter implements Converter {
    @Override
    public Object convert(Object raw) {
        String loaderType = Objects.toString(raw, LoaderType.ANVIL.name());
        return LoaderType.valueOf(loaderType.toUpperCase());
    }

    @Override
    public Object convertToRaw(Object value) {
        LoaderType loaderType = (LoaderType) value;
        return loaderType.name();
    }
}
