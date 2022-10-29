import path from 'path';
import fs from 'fs';

export const joinPath = start => end => path.join(start,end);

export const joinPathImpl = (start, end) => path.join(start,end);

export const resolveImpl = (paths, path_) => () => path.resolve.apply(this, paths.concat(path_));

export const readTextFileImpl = path => (onError, onSuccess) => {
  fs.readFile(path, { encoding: 'UTF-8' }, (err, data) => err ? onError(err) : onSuccess(data));
  return (cancelError, onCancelerError, onCancelerSuccess) => onCancelerSuccess();
}
