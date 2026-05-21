export type DecodeResult<T> =
  | { ok: true; value: T }
  | { ok: false; error: string };

export type Decoder<T> = (value: unknown, path?: string) => DecodeResult<T>;

export const ok = <T>(value: T): DecodeResult<T> => ({ ok: true, value });

export const err = <T = never>(error: string): DecodeResult<T> => ({ ok: false, error });

export const stringValue: Decoder<string> = (value, path = 'value') =>
  typeof value === 'string' ? ok(value) : err(`${path} must be a string`);

export const booleanValue: Decoder<boolean> = (value, path = 'value') =>
  typeof value === 'boolean' ? ok(value) : err(`${path} must be a boolean`);

export const numberValue: Decoder<number> = (value, path = 'value') =>
  typeof value === 'number' && Number.isFinite(value) ? ok(value) : err(`${path} must be a finite number`);

export const unknownRecord = (value: unknown, path = 'value'): DecodeResult<Record<string, unknown>> =>
  value !== null && typeof value === 'object' && !Array.isArray(value)
    ? ok(value as Record<string, unknown>)
    : err(`${path} must be an object`);

export const arrayOf = <T>(decoder: Decoder<T>): Decoder<T[]> => (value, path = 'value') => {
  if (!Array.isArray(value)) {
    return err(`${path} must be an array`);
  }
  const decoded: T[] = [];
  for (let index = 0; index < value.length; index += 1) {
    const item = decoder(value[index], `${path}[${String(index)}]`);
    if (!item.ok) {
      return item;
    }
    decoded.push(item.value);
  }
  return ok(decoded);
};

export const field = <T>(record: Record<string, unknown>, name: string, decoder: Decoder<T>, path: string): DecodeResult<T> =>
  decoder(record[name], `${path}.${name}`);

export const optionalField = <T>(
  record: Record<string, unknown>,
  name: string,
  decoder: Decoder<T>,
  path: string
): DecodeResult<T | undefined> => {
  if (!(name in record) || record[name] === null) {
    return ok(undefined);
  }
  return decoder(record[name], `${path}.${name}`);
};
